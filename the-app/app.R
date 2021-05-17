#!/usr/local/bin//Rscript --vanilla


library(shiny)
library(shinydashboard)
library(shinyBS)
library(data.table)
library(stringr)
library(DT)
library(ggplot2)
library(forecast)
library(glue)


options(warn=1)
options(scipen=20)



###### TEMPORARY #################################
###### TEMPORARY #################################
### this is temporary until libbib >= v1.6.2
### is on CRAN

set_lb_attribute <- function(x, type, value){
  setattr(x, sprintf("lb.%s", type), value)
}

set_lb_date <- function(x, value){
  if("Date" %chin% class(value))
    set_lb_attribute(x, "date", value)
  else
    set_lb_attribute(x, "date", as.Date(value))
}

cp_lb_attributes <- function(a, b){
  tmp <- names(attributes(a))
  tmp <- stringr::str_subset(tmp, "^lb\\.")
  invisible(sapply(tmp, function(x) setattr(b, x, attr(a, x))))
}

split_extension <- function(fname){
  if(length(fname)>1) stop("only takes one filename")
  dirpart <- sprintf("%s/", dirname(fname))
  if(dirpart=="./") dirpart <- ""
  basepart <- basename(fname)
  pieces <- stringr::str_split(basepart, "\\.", n=2)
  before_ext <- pieces[[1]][1]
  after_ext <- pieces[[1]][2]
  if(!is.na(after_ext)) after_ext <- sprintf(".%s", after_ext)
  if(is.na(before_ext) & is.na(after_ext)) stop("couldn't split extension")
  c(sprintf("%s%s", dirpart, before_ext), after_ext)
}

fread_plus_helper <- function(fname){
  pieces <- split_extension(fname)
  file.match <- Sys.glob(sprintf("%s*%s", pieces[1], pieces[2]))
  if(length(file.match)==0) stop("no matching filename")
  if(length(file.match)>1){
    tmp <- sprintf("\n  %s", paste(file.match, collapse=", "))
    stop("more than one matching file: ", tmp)
  }
  # will use the native pipe operator when everone uses > R v4.1
  plus.part <- stringr::str_replace(file.match, sprintf("^%s\\D*", pieces[1]), "")
  plus.part <- stringr::str_replace(plus.part,  sprintf("\\D*%s$", pieces[2]), "")

  list(matching_filename=file.match,
       given_filename=fname,
       plus_part=plus.part,
       before_ext=pieces[1],
       after_ext=pieces[2])
}

fread_plus_date <- function(fname, allow.fallback.date=TRUE, ...){
  dat <- NULL
  helper.ret  <- fread_plus_helper(fname)
  already.date <- stringr::str_extract(helper.ret$before_ext,
                                       "\\d{4}-\\d{2}-\\d{2}$")
  if(!is.na(already.date)){
    # already has the date in the filename
    fname.to.use <- fname
    thedate <- as.Date(already.date)
  } else{
    # doesn't already have the date in the filename
    fname.to.use <- helper.ret$matching_filename
    plus_part <- helper.ret$plus_part
    if(plus_part==""){
      if(!allow.fallback.date)
        stop("no date in filename found")
      message("no date in filename found... using today's date")
      thedate <- Sys.Date()
    } else {
      thedate <- as.Date(plus_part)
    }
  }
  dat <- fread(fname.to.use, ...)
  set_lb_attribute(dat, "date", thedate)
  dat
}
###### END TEMPORARY #############################
###### END TEMPORARY #############################


# --------------------------------------------------------- #
# HELPER FUNCTIONS
# --------------------------------------------------------- #

add.perc.to.names <- function(twocoldt){
  tmp <- as.vector(unlist(100 * (twocoldt[, 2] / sum(twocoldt[, 2]))))
  formernames <- as.vector(unlist(twocoldt[,1]))
  newnames <- sprintf("%s (%.1f%%)", formernames, tmp)
  twocoldt[[1]] <- newnames
  return(twocoldt)
}

compress <- function(twocoldt, n){
  # HAS TO BE SORTED!!!
  one <- twocoldt[1:(n-1),]
  twocoldt[n, 1] <- "Other"
  twocoldt[n, 2] <- sum(twocoldt[n:.N, 2])
  almost <- rbind(one, twocoldt[n,])
  return(add.perc.to.names(almost))
}

make_desc_text <- function(dataused){
  text <- ""
  lbdate <- attr(dataused, "lb.date")
  lbsource <- attr(dataused, "lb.source")
  lbnotes <- attr(dataused, "lb.note")
  if(!is.null(lbsource))
    text <- sprintf("%s<b>raw data sourced from:</b> %s", text, lbsource)
  if(!is.null(lbdate))
    text <- sprintf("%s<br><b>source last updated:</b> %s", text, as.character(lbdate))
  if(!is.null(lbnotes))
    text <- sprintf("%s<br><b>notes:</b> %s", text, lbnotes)
  text
}

make_popover <- function(id, text){
  bsPopover(id=id, title="meta-info", content=text,
            trigger="click", placement="bottom")
}

make_popover_with_attributes <- function(id, dataused){
  text <- make_desc_text(dataused)
  make_popover(id=id, text=text)
}


make_link <- function(link){
  glue('<a href="{link}">{link}</a>')
}

make_small <- function(text, small=3){
  nowitssmall <- sprintf("%s%s%s", glue_collapse(rep("<small>", small)),
                                   text,
                                   glue_collapse(rep("</small>", small)))
  nowitssmall
}

# --------------------------------------------------------- #
# --------------------------------------------------------- #


# --------------------------------------------------------- #
# LOADING DATA AND SETTING ATTRIBUTES
# --------------------------------------------------------- #

# - will be deprecated
nicecenterinfo <- fread_plus_date("./data/nice-wmr-and-lair-quarterly-stats.txt",
                                  sep="\t", header=TRUE)
set_lb_attribute(nicecenterinfo, "source",
                 glue('<br>{make_link("http://ilsstaff.nypl.org/iii/webrpt/UserLogin.html")} and<br>{make_link("https://cap.apps.nypl.org/do/rs_viewers/view_readers_materials")}'))
setorder(nicecenterinfo, FY, quarter)
nicecenterinfo[, period:=nicecenterinfo[, sprintf("FY%s-Q%d", as.character(FY),
                                                  quarter)]]
nicetotals <- nicecenterinfo[, .(totals=sum(circ, na.rm=TRUE)), .(FY, quarter)]
niceyeartotals <- nicetotals[, sum(totals), FY]
cp_lb_attributes(nicecenterinfo, nicetotals)
cp_lb_attributes(nicecenterinfo, niceyeartotals)

# ---

recapgeninfo <- fread_plus_date("./data/recap-gen-info.dat",
                                sep="\t", header=TRUE)
set_lb_attribute(recapgeninfo, "source", "SCSB MARCXml export")
set_lb_attribute(recapgeninfo, "note", glue('derived from data substrate from {make_link("https://github.com/recap-assessment-team/compile-recap-stats")}'))

# ---

visitinfo <- fread_plus_date("./data/visits-by-quarter.dat", sep="\t", header=TRUE)
set_lb_attribute(visitinfo, "source", "https://lair.nypl.org/-/departments/library-sites-and-services/research-libraries/view-statistics")
set_lb_attribute(visitinfo, "note", "this source of data is now deprecated")

# ---

geninfo <- fread_plus_date("./data/gen-info.dat", sep="\t", header=TRUE)
set_lb_attribute(geninfo, "source", "Sierra shadow database")
set_lb_attribute(geninfo, "note", glue('derived from the data product produced by {make_link("https://github.com/NYPL/sierra-shadow-dataset")}'))
langinfo <- fread_plus_date("./data/langinfo.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, langinfo)
langnorm <- fread_plus_date("./data/langnorm.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, langnorm)
locationinfo <- fread_plus_date("./data/centerinfo.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, locationinfo)
mattypeinfo <- fread_plus_date("./data/xmattype.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, mattypeinfo)
biblevelinfo <- fread_plus_date("./data/xbiblevel.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, biblevelinfo)
countriesinfo  <- fread_plus_date("./data/countryinfo.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, countriesinfo)
locationinfo <- fread_plus_date("./data/centerinfo.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, locationinfo)
lc1info <- fread_plus_date("./data/lc1-info.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, lc1info)
lc1info[, short_desc:=ifelse(str_length(lc1info[, lc_subject_class])>20,
                             sprintf("(%s) %s...",
                                     lc1info[, first_letter],
                                     str_sub(lc1info[, lc_subject_class], 1, 17)),
                             sprintf("(%s) %s",
                                     lc1info[, first_letter],
                                     lc1info[, lc_subject_class]))]
lc1norm <- fread_plus_date("./data/lc1-norm.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, lc1norm)
lc1norm[, short_desc:=ifelse(str_length(lc1norm[, lc_subject_class])>20,
                             sprintf("(%s) %s...",
                                     lc1norm[, first_letter],
                                     str_sub(lc1norm[, lc_subject_class], 1, 17)),
                             sprintf("(%s) %s",
                                     lc1norm[, first_letter],
                                     lc1norm[, lc_subject_class]))]
lc2info <- fread_plus_date("./data/lc2-info.dat", sep="\t", header=TRUE)
cp_lb_attributes(geninfo, lc2info)

# ---

sandddaily  <- fread_plus_date("./data/scan-and-deliver-daily.dat", sep='\t', header=TRUE)
setnames(sandddaily, "xdate", "thetime")
set_lb_attribute(sandddaily, "source", "https://docs.google.com/spreadsheets/d/13zzPYWSM4YTeBfApgVdgdpEZWaIdk_Bu2io5JQqS0KY/edit#gid=0")
set_lb_attribute(sandddaily, "note", "derived from the data product produced by https://github.com/NYPL/scan-and-deliver-stats")
sanddweekly <- fread_plus_date("./data/scan-and-deliver-weekly.dat", sep='\t', header=TRUE)
setnames(sanddweekly, "xdate", "thetime")
cp_lb_attributes(sandddaily, sanddweekly)
sanddlang <- fread_plus_date("./data/scan-and-deliver-language-breakdown.dat",
                             sep="\t", header=TRUE)
cp_lb_attributes(sandddaily, sanddlang)
sanddlc1 <- fread_plus_date("./data/scan-and-deliver-subject-classification-breakdown.dat",
                            sep="\t", header=TRUE)
cp_lb_attributes(sandddaily, sanddlc1)

# ---

ezprox1 <- fread_plus_date("./data/ezproxy-vendor-dates.dat", sep='\t', header=TRUE)
set_lb_attribute(ezprox1, "source", "raw HTTP logs from EZ-Proxy Web Server")
set_lb_attribute(ezprox1, "note", "derived from the data product produced by https://github.com/NYPL/ezproxy-stats")

# --------------------------------------------------------- #
# --------------------------------------------------------- #



#############################################################
#############################################################
# THE UI
#############################################################

header <- dashboardHeader(
  title = "NYPL Research",
  dropdownMenu(type = "notifications",
               notificationItem(
                 text="See footnotes at bottom of main tab",
                 icon=icon("exclamation-triangle"),
                 status="warning")),
  dropdownMenu(type = "tasks",
               taskItem(value=5, color="red",
                        HTML('Add Counter stats to dashboard')
                        ),
               taskItem(value=20, color="yellow",
                        HTML("Finish automation of LC Call<br> Number cross-pollination")
                        ),
               taskItem(value=10, color="yellow",
                        HTML("Rollup 'place of publication' to 'country' level")
                        )
              )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),

    menuItem("Research Centers", tabName = "location", icon = icon("r-project"),
             menuSubItem("General info", tabName="locationgen", icon=icon("pie-chart"))),

    menuItem("Circulation", tabName = "circsuper", icon = icon("bezier-curve"),
             menuSubItem("Overall quarterly circ", tabName="circoverallview", icon=icon("line-chart")),
             menuSubItem("Circ by center", tabName="circbycenter", icon=icon("line-chart")),
             menuSubItem(icon=NULL,
               selectInput("centeropt", "Center:",
                                     c("SASB" = "SASB",
                                       "Schomburg" = "Schomburg",
                                       "LPA" = "LPA"
                                       )))),

    menuItem("Visits", tabName = "visitssuper", icon = icon("book-reader"),
             menuSubItem("Visits Overview", tabName="visitssuboverview", icon=icon("dashboard"))),

    menuItem("Bib Level", tabName = "biblevelsuper", icon = icon("couch"),
             menuSubItem("Raw data table", tabName="biblevelsubraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="biblevelsubpie", icon=icon("pie-chart"))),

    menuItem("Material Type", tabName = "mattypesuper", icon = icon("kiwi-bird"),
             menuSubItem("Raw data table", tabName="mattypesubraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="mattypesubpie", icon=icon("pie-chart"))),

    menuItem("Language", tabName = "languagesuper", icon = icon("language"),
             menuSubItem("Raw data table", tabName="languagesubraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="languagesubpie", icon=icon("pie-chart")),
             menuSubItem("Explorer", tabName="languagesubexplorer", icon=icon("search-plus"))),

    menuItem("Place of Publication", tabName = "pubplacesuper", icon = icon("globe-americas"),
             menuSubItem("Raw data table", tabName="pubplacesubraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="pubplacesubpie", icon=icon("pie-chart"))
    ),

    menuItem("LC subject (I)", tabName = "lc1super", icon = icon("landmark"),
             menuSubItem("Raw data table", tabName="lc1subraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="lc1subpie", icon=icon("pie-chart")),
             menuSubItem("Explorer", tabName="lc1subexplorer", icon=icon("search-plus"))
    ),

    menuItem("LC subject (II)", tabName = "lc2super", icon = icon("landmark"),
             menuSubItem("Raw data table", tabName="lc2subraw", icon=icon("table"))
    ),

    menuItem("Scan and deliver", tabName = "sanddsuper", icon = icon("truck"),
             menuSubItem("Requests", tabName="sanddreqstab", icon=icon("line-chart")),
             menuSubItem("Language breakdown", tabName="sanddlangtab", icon=icon("table")),
             menuSubItem("LC subject breakdown", tabName="sanddlc1tab", icon=icon("table"))
    ),

    menuItem("Electronic Resources", tabName = "ertab", icon = icon("laptop"),
             menuSubItem("EZ proxy", tabName="ezproxytab",
                         icon=icon("line-chart")),
             menuSubItem("Counter", tabName="countertab",
                         icon=icon("laptop"))
    ),

    menuItem("Aeon", tabName = "aeontab", icon = icon("palette")
    )

  )
)

body <- dashboardBody(
  tabItems(

  # --------------------------------------------------------- #
  # Overview
  # --------------------------------------------------------- #
    tabItem(tabName = "overview",
            h1("Overview"),
            br(),
            fluidRow(
              valueBoxOutput("totalItemsValueBox"),
              make_popover_with_attributes("totalItemsValueBox", geninfo),
              valueBoxOutput("totalBibsValueBox"),
              make_popover_with_attributes("totalBibsValueBox", geninfo),
            ),
            br(),
            fluidRow(
              valueBoxOutput("recapNewItemsValueBox"),
              make_popover_with_attributes("recapNewItemsValueBox", recapgeninfo),
              valueBoxOutput("recapNewBibsValueBox"),
              make_popover_with_attributes("recapNewBibsValueBox", recapgeninfo)
            ),
            br(),
            fluidRow(
              valueBoxOutput("fy18checkoutsValueBox"),
              make_popover_with_attributes("fy18checkoutsValueBox", niceyeartotals),
              valueBoxOutput("fy19checkoutsValueBox"),
              make_popover_with_attributes("fy19checkoutsValueBox", niceyeartotals),
              valueBoxOutput("fy20checkoutsValueBox"),
              make_popover_with_attributes("fy20checkoutsValueBox", niceyeartotals)
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                          "*    does not include shared collection items from other institutions",
                          br(),
                          br(),
                          "†    does not include new Harvard integration candidates (potentially 3.8 million titles)",
                          br(),
                          br(),
                          "‡    This includes all Sierra checkouts usage as reported on the Web Management Report and the Cloud Apps Portal",
                          width=7))
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Locations
  # --------------------------------------------------------- #
    tabItem(tabName = "locationgen",
            h1("Research Centers"),
            downloadButton("downloadcenterrawdata", "Download"),
            br(),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       footer=HTML(make_small(glue('{make_desc_text(locationinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       DTOutput('centertable'),
                       width=12)
              )
            ),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items at each center",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("locationpie"), width=12),
                     make_popover_with_attributes("locationpie", locationinfo)
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Circulations
  # --------------------------------------------------------- #
    tabItem(tabName = "circoverallview",
            h1("Overall Quarterly Circulation"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Overall circ by quarter",
                       footer=HTML(make_small(glue('{make_desc_text(nicetotals)}'))),
                       collapsible = TRUE,
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("totalquarterlycircs"), width=12))),
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Circ by center
  # --------------------------------------------------------- #
    tabItem(tabName = "circbycenter",
            h1("Circulation by research center"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Quarterly circulation by center",
                       footer=HTML(make_small(glue('{make_desc_text(nicecenterinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("quarterlycircbycenter"), width=12)
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Visits
  # --------------------------------------------------------- #
    tabItem(tabName = "visitssuboverview",
            h1("Visits overview"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Visits by quarter",
                       footer=HTML(make_small(glue('{make_desc_text(visitinfo)}'))),
                       collapsible = TRUE,
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("totalquarterlyvisits"), width=12)
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Bib level
  # --------------------------------------------------------- #
    tabItem(tabName = "biblevelsubraw",
            h1("Bib Level"),
            downloadButton("downloadbiblevelrawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       DTOutput('bibleveltable'),
                       width=12
                     )
              )
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(biblevelinfo)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "biblevelsubpie",
            h1("Bib Level"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items at each bib level",
                       footer=HTML(make_small(glue('{make_desc_text(biblevelinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("biblevelpie"), width=9),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienumbiblevel", "Number of top bib levels:", 2, 8, 3),
                       width=3)
              )
            )
    ),

  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Material type
  # --------------------------------------------------------- #
    tabItem(tabName = "mattypesubraw",
            h1("Material Type"),
            downloadButton("downloadmattyperawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                      title="Tabular info",
                      solidHeader = TRUE,
                      status="primary",
                      DTOutput('mattypetable'),
                      width=12
                     )
              )
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(mattypeinfo)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "mattypesubpie",
            h1("Material Type"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each material type catagory",
                       footer=HTML(make_small(glue('{make_desc_text(mattypeinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("mattypepie"),
                       width=9
                       ),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienummattype", "Number of top material types:", 2, 17, 5),
                       width=3)
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Language
  # --------------------------------------------------------- #
    tabItem(tabName = "languagesubraw",
            h1("Language"),
            downloadButton("downloadlangrawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       DTOutput('languagetable'),
                       width=12
                     )
              )
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(langinfo)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "languagesubpie",
            h1("Language"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each language",
                       footer=HTML(make_small(glue('{make_desc_text(langinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("languagepie"),
                       width=9
                     ),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienumlangs", "Number of top languages:", 2, 50, 5),
                       width=3)
              )
            )
    ),

    tabItem(tabName = "languagesubexplorer",
      h1("Language (explorer)"),
      br(),
      fluidRow(
        column(width=9,
                box(
                  title="Strength explorer (play with the strength coefficients)",
                  footer=HTML(make_small(glue('{make_desc_text(langinfo)}'))),
                  status="primary",
                  solidHeader = TRUE,
                  plotOutput("languageexplorerbar"),
                  width=12
                )
        ),
        column(width=3,
          box(
            title="Limit",
            status="primary",
            solidHeader = TRUE,
            sliderInput("langexplorer_limit", "Number of top languages:", 1, 30, 5),
            width=12
          ),
          box(
            title = "Strength coefficients",
            solidHeader = TRUE,
            status="primary",
            sliderInput("langexplore_numitems", "Number of items:", 0, 1, 1),
            sliderInput("langexplore_rawcirc", "Raw circulation:", 0, 1, 0),
            sliderInput("langexplore_controlledcirc", "Circs per item:", 0, 1, 0),
            sliderInput("langexplore_datediv", "Pub year diversity:", 0, 1, 0),
            width=12
          )
        )
      )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Place of publication
  # --------------------------------------------------------- #
    tabItem(tabName = "pubplacesubraw",
            h1("Place of publication"),
            downloadButton("downloadpubplacerawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       DTOutput('countriestable'),
                       width=12
                     )
              )
            ),
            #br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(countriesinfo)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "pubplacesubpie",
            h1("Place of publication"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items published in place",
                       footer=HTML(make_small(glue('{make_desc_text(countriesinfo)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("countriespie"),
                       width=9),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienumcountries", "Number of top places:", 2, 50, 5),
                       width=3)
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Library of congress subject classifications
  # --------------------------------------------------------- #
    tabItem(tabName = "lc1subraw",
            h1("Library of Congress subject classifications"),
            downloadButton("downloadlc1rawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       DTOutput('lc1table'),
                       width=12
                     )
              )
            ),
            #br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(lc1info)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "lc1subpie",
            h1("Library of Congress subject classifications"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each subject category",
                       footer=HTML(make_small(glue('{make_desc_text(lc1info)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("lc1pie"),
                       width=9
                     ),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienumlc1", "Number of top subject categories:", 2, 21, 5),
                       width=3)
              )
            )
    ),

    tabItem(tabName = "lc1subexplorer",
            h1("Library of Congress subject classifications [explorer]"),
            br(),
            fluidRow(
              column(width=9,
                     box(
                       title="Strength explorer (play with the strength coefficients)",
                       footer=HTML(make_small(glue('{make_desc_text(lc1info)}'))),
                       status="primary",
                       solidHeader = TRUE,
                       plotOutput("lc1explorerbar"),
                       width=12
                     )
              ),
              column(width=3,
                     box(
                       title="Limit",
                       status="primary",
                       solidHeader = TRUE,
                       sliderInput("lc1explorer_limit", "Number of top subject categories:", 1, 21, 5),
                       width=12
                     ),
                     box(
                       title = "Strength coefficients",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("lc1explore_numitems", "Number of items:", 0, 1, 1),
                       sliderInput("lc1explore_rawcirc", "Raw circulation:", 0, 1, 0),
                       sliderInput("lc1explore_controlledcirc", "Circs per item:", 0, 1, 0),
                       sliderInput("lc1explore_datediv", "Pub year diversity:", 0, 1, 0),
                       width=12
                     )
              )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Library of congress subject subclassifications
  # --------------------------------------------------------- #
    tabItem(tabName = "lc2subraw",
            h1("Library of Congress subject subclassifications"),
            downloadButton("downloadlc2rawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       DTOutput('lc2table'),
                       width=12
                     )
              )
            ),
            #br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(lc2info)),
                         width=7)
                     )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Scan and deliver
  # --------------------------------------------------------- #
    tabItem(tabName = "sanddreqstab",
            h1("Scan and deliver"),
            br(),
            fluidRow(
              column(width=9,
                     box(
                       title="Number of filled requests",
                       footer=HTML(make_small(glue('{make_desc_text(sandddaily)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("sanddreqsplot"),
                       width=12,
                     ),
              ),
              column(width=3,
                     box(
                       title="Center",
                       status="primary",
                       solidHeader = TRUE,
                       selectInput("sanddcenteropt", "Choose center:",
                                   c(
                                     "All Centers" = "total",
                                     "LPA" = "LPA",
                                     "Schomburg" = "Schomburg",
                                     "SASB" = "SASB"
                                   )),
                       width=12
                     ),
                     box(
                       title="Frequency",
                       status="primary",
                       solidHeader = TRUE,
                       selectInput("sanddfreqopt", "Choose frequency:",
                                   c("Daily" = "Daily",
                                     "Weekly" = "Weekly"
                                   )),
                       width=12
                     ),
                     box(
                       title="Smoothing",
                       status="primary",
                       solidHeader = TRUE,
                       selectInput("sanddsmoothp", "Loess smoothing?:",
                                   c("None" = "None",
                                     "Yes" = "Yes"
                                   )),
                       width=12
                     )

              )
            )
    ),

    tabItem(tabName = "sanddlangtab",
            h1("Scan and deliver (language breakdown)"),
            downloadButton("downloadsanddlangrawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       DTOutput('sanddlangtable'),
                       width=12
                     )
              )
            ),
            #br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(sanddlang)),
                         width=7)
                     )
            )
    ),

    tabItem(tabName = "sanddlc1tab",
            h1("Scan and deliver (LC Subject Class breakdown)"),
            downloadButton("downloadsanddlc1rawdata", "Download"),
            br(), br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       DTOutput('sanddlc1table'),
                       width=12
                     )
              )
            ),
            #br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         HTML(make_desc_text(sanddlc1)),
                         width=7)
                     )
            )
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # EZ Proxy
  # --------------------------------------------------------- #
    tabItem(tabName = "ezproxytab",
            h1("EZ proxy statistics"),
            br(), br(),
            fluidRow(
              column(width=9,
                     box(
                       title="Number of unique sessions over time",
                       footer=HTML(make_small(glue('{make_desc_text(ezprox1)}'))),
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("ezuniqsessionsplot"),
                       width=12,
                     ),
              ),
              column(width=3,
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("eztopvendors", "Number of top vendors:", 1, 12, 5),
                       width=12
                     ),
                     box(
                       title="Include grand total",
                       status="primary",
                       solidHeader = TRUE,
                       selectInput("eztotalp", "Include grand total:",
                                   c("No" = "No",
                                     "Yes" = "Yes"
                                   )),
                       width=12
                     ),
                     box(
                       title="Smoothing",
                       status="primary",
                       solidHeader = TRUE,
                       selectInput("ezsmoothp", "GAM smoothing?:",
                                   c("None" = "None",
                                     "Yes" = "Yes"
                                   )),
                       width=12
                     )

              )
            )

    ),

    tabItem(tabName = "countertab",
            h1("Counter Statistics"),
            br(), br(),
            "forthcoming"
    ),
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Aeon
  # --------------------------------------------------------- #

    tabItem(tabName = "aeontab",
            h1("Aeon statistics"),
            br(), br(),
            "forthcoming"
    )
  # --------------------------------------------------------- #




  )
)

ui <- dashboardPage(header, sidebar, body)




#############################################################
#############################################################
# THE SERVER
#############################################################

server <- function(input, output) {

  # --------------------------------------------------------- #
  # OVERVIEW                                                  #
  # --------------------------------------------------------- #
  output$totalItemsValueBox <- renderValueBox({
    valueBox(
      prettyNum(as.integer(geninfo[dakey=="num_uniq_items", davalue]), big.mark=","),
      "Total barcoded items in collection*",
      color="purple",
      icon=icon("book")
    )
  })

  output$totalBibsValueBox <- renderValueBox({
    valueBox(
      prettyNum(as.integer(geninfo[dakey=="num_uniq_bibs", davalue]), big.mark=","),
      "Total number of titles in collection*",
      color="purple",
      icon=icon("book")
    )
  })

  output$recapNewItemsValueBox <- renderValueBox({
    valueBox(
      prettyNum(recapgeninfo[variable=="non-nypl-items", value], big.mark=","),
      "Newly accessible non-NYPL items available through Shared Collection†",
      color="red",
      icon=icon("dolly-flatbed")
    )
  })

  output$recapNewBibsValueBox <- renderValueBox({
    valueBox(
      prettyNum(recapgeninfo[variable=="non-nypl-titles", value], big.mark=","),
      "Newly accessible non-NYPL titles available through Shared Collection†",
      color="red",
      icon=icon("dolly-flatbed")
    )
  })

  output$fy18checkoutsValueBox <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==18, V1], big.mark=","),
      "Checkouts in FY18‡",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$fy19checkoutsValueBox <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==19, V1], big.mark=","),
      "Checkouts in FY19‡",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$fy20checkoutsValueBox <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==20, V1], big.mark=","),
      "Checkouts in FY20‡ (to Q3)",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$totalquarterlycircs <- renderPlot({
    quarterly_totals <- ts(nicetotals[, totals], start=2017, frequency=4)
    autoplot(quarterly_totals) + ylab("quarterly totals") + xlab("fiscal year")
    autoplot(quarterly_totals, color="blue", size=1.4, alpha=0.6) + ylab("quarterly totals") + xlab("fiscal year")
  })

  output$totalquarterlyvisits <- renderPlot({
    quarterly_visits <- ts(visitinfo[, visits], start=2008, frequency=4)
    autoplot(quarterly_visits, color="blue", size=1.4, alpha=0.6) + ylab("quarterly visits") + xlab("fiscal year")
  })


  output$fy18checkoutsValueBox2 <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==18, V1], big.mark=","),
      "Checkouts in FY18‡",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$fy19checkoutsValueBox2 <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==19, V1], big.mark=","),
      "Checkouts in FY19‡",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$fy20checkoutsValueBox2 <- renderValueBox({
    valueBox(
      prettyNum(niceyeartotals[FY==20, V1], big.mark=","),
      "Checkouts in FY20‡ (to Q3)",
      color="yellow",
      icon=icon("exchange")
    )
  })

  output$downloadcircrawdata <- downloadHandler(
    filename = "raw-circ-data.txt",
    content = function(file){
      fwrite(nicecenterinfo, file, sep="\t")
    }
  )
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # CENTER/LOCATION                                           #
  # --------------------------------------------------------- #
  output$centertable <- {
    tmp5 <- copy(locationinfo)
    setorder(locationinfo, -num_bibs)
    setorder(tmp5, -num_bibs)
    tmp5[, controlled_circ:=sprintf("%.3f", controlled_circ)]
    tmp5[, percent_coll:=sprintf("%.3f%%", 100*percent_coll)]
    tmp5[, num_items:=prettyNum(num_items, big.mark=",")]
    tmp5[, num_bibs:=prettyNum(num_bibs, big.mark=",")]
    tmp5[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp5, "num_items", "Item Count")
    setnames(tmp5, "num_bibs", "Bib Count")
    setnames(tmp5, "total_circ", "Total Circ")
    setnames(tmp5, "controlled_circ", "Circ Per Item")
    setnames(tmp5, "percent_coll", "Percent of collection")
    tmp5[center=="SIBL", center:="Business Library"]
    renderDT(tmp5)
  }

  output$downloadcenterrawdata <- downloadHandler(
    filename = "raw-location-data.txt",
    content = function(file){
      fwrite(locationinfo, file, sep="\t")
    }
  )

  output$locationpie <- renderPlot({
    setorder(locationinfo, -num_items)
    tmp <- add.perc.to.names(locationinfo[, .(center, num_items)])
    pie(tmp$num_items, labels=tmp$center)
  })


  output$quarterlycircbycenter <- renderPlot({
    theone <- input$centeropt
    tmp <- nicecenterinfo[center==theone]
    tmp[collection=="general", collection:="web management report"]
    tmp[collection=="special", collection:="readers and materials consulted"]
    ggplot(tmp, aes(x=period, y=circ, group=collection, fill=collection, color=collection)) +
      geom_line() +
      ggtitle(sprintf("%s circulation and materials consulted", theone))

  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # BIB LEVEL                                                 #
  # --------------------------------------------------------- #
  output$bibleveltable <- {
    tmp3 <- copy(biblevelinfo)
    setorder(tmp3, -N)
    setorder(biblevelinfo, -N)
    tmp3[, N:=prettyNum(N, big.mark=",")]
    setnames(tmp3, "biblevel", "Bib Level")
    setnames(tmp3, "N", "Total")
    tmp3[, percent_coll:=NULL]
    renderDT(tmp3)
  }

  output$downloadbiblevelrawdata <- downloadHandler(
    filename = "raw-biblevel-data.txt",
    content = function(file){
      fwrite(biblevelinfo, file, sep="\t")
    }
  )

  output$biblevelpie <- renderPlot({
    setorder(biblevelinfo, -N)
    tmp <- compress(biblevelinfo[, .(biblevel, N)], input$pienumbiblevel)
    pie(tmp$N, labels=tmp$biblevel)
  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # MATERIAL TYPE                                             #
  # --------------------------------------------------------- #
  output$mattypetable <- {
    tmp2 <- copy(mattypeinfo)
    setorder(tmp2, -N)
    setorder(mattypeinfo, -N)
    tmp2[, N:=prettyNum(N, big.mark=",")]
    setnames(tmp2, "mattype", "Material Type")
    setnames(tmp2, "N", "Total")
    tmp2[, percent_coll:=NULL]
    renderDT(tmp2)
  }

  output$downloadmattyperawdata <- downloadHandler(
    filename = "raw-material-type-data.txt",
    content = function(file){
      fwrite(mattypeinfo, file, sep="\t")
    }
  )

  output$mattypepie <- renderPlot({
    setorder(mattypeinfo, -N)
    tmp <- compress(mattypeinfo[, .(mattype, N)], input$pienummattype)
    pie(tmp$N, labels=tmp$mattype)
  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # LANGUAGES                                                 #
  # --------------------------------------------------------- #
  output$languagetable <- {
    tmp <- copy(langinfo)
    setorder(langinfo, -itemcount)
    setorder(tmp, -itemcount)
    tmp[, controlled_circ:=sprintf("%.2f", controlled_circ)]
    tmp[, percent_coll:=sprintf("%.2f%%", 100*percent_coll)]
    tmp[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp[, fy19_circ:=prettyNum(fy19_circ, big.mark=",")]
    tmp[, fy20_circ:=prettyNum(fy20_circ, big.mark=",")]
    tmp[, fy21_circ:=prettyNum(fy21_circ, big.mark=",")]
    tmp[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp, "itemcount", "Item Count")
    setnames(tmp, "bibcount", "Bib Count")
    setnames(tmp, "date.div", "Date Spread")
    setnames(tmp, "fy17_circ", "FY17 Circ")
    setnames(tmp, "fy18_circ", "FY18 Circ")
    setnames(tmp, "fy19_circ", "FY19 Circ")
    setnames(tmp, "fy20_circ", "FY20 Circ")
    setnames(tmp, "fy21_circ", "FY21 Circ")
    setnames(tmp, "total_circ", "Total Circ")
    setnames(tmp, "controlled_circ", "Circ Per Item")
    setnames(tmp, "percent_coll", "Percent of collection")
    renderDT(tmp)
  }

  output$downloadlangrawdata <- downloadHandler(
    filename = "raw-language-data.txt",
    content = function(file){
      fwrite(langinfo, file, sep="\t")
    }
  )

  output$languagepie <- renderPlot({
    setorder(langinfo, -itemcount)
    tmp <- compress(langinfo[, .(language, itemcount)], input$pienumlangs)
    pie(tmp$itemcount, labels=tmp$language)
  })

  output$languageexplorerbar <- renderPlot({
    tmpmat <- as.matrix(langnorm[, 3:6])
    coeffs <- c(input$langexplore_numitems,
                input$langexplore_datediv,
                input$langexplore_rawcirc,
                input$langexplore_controlledcirc)
    coeffmat <- matrix(coeffs, ncol=1)
    tmp <- tmpmat %*% coeffmat
    res <- data.table(language=ifelse(str_length(langnorm[, language])>17,
                                      sprintf("%s...", str_sub(langnorm[, language], 1, 14)),
                                      langnorm[, language]),
                      score=as.vector(tmp))
    setorder(res, -score)
    res <- res[1:input$langexplorer_limit,]
    barplot(res[, score], names.arg=res[, language], yaxt="n")
  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Place of publication                                      #
  # --------------------------------------------------------- #
  output$countriestable <- {
    tmp8 <- copy(countriesinfo)
    setorder(countriesinfo, -itemcount)
    setorder(tmp8, -itemcount)
    tmp8[, controlled_circ:=sprintf("%.2f", controlled_circ)]
    tmp8[, percent_coll:=sprintf("%.2f%%", 100*percent_coll)]
    tmp8[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp8[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp8[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp8[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp8[, fy19_circ:=prettyNum(fy19_circ, big.mark=",")]
    tmp8[, fy20_circ:=prettyNum(fy20_circ, big.mark=",")]
    tmp8[, fy21_circ:=prettyNum(fy21_circ, big.mark=",")]
    tmp8[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp8, "itemcount", "Item Count")
    setnames(tmp8, "bibcount", "Bib Count")
    setnames(tmp8, "date.div", "Date Spread")
    setnames(tmp8, "fy17_circ", "FY17 Circ")
    setnames(tmp8, "fy18_circ", "FY18 Circ")
    setnames(tmp8, "fy19_circ", "FY19 Circ")
    setnames(tmp8, "fy20_circ", "FY20 Circ")
    setnames(tmp8, "fy21_circ", "FY21 Circ")
    setnames(tmp8, "total_circ", "Total Circ")
    setnames(tmp8, "controlled_circ", "Circ Per Item")
    setnames(tmp8, "percent_coll", "Percent of collection")
    renderDT(tmp8)
  }

  output$downloadpubplacerawdata <- downloadHandler(
    filename = "raw-publication-place-data.txt",
    content = function(file){
      fwrite(countriesinfo, file, sep="\t")
    }
  )

  output$countriespie <- renderPlot({
    setorder(countriesinfo, -itemcount)
    tmp <- compress(countriesinfo[, .(country, itemcount)], input$pienumcountries)
    pie(tmp$itemcount, labels=tmp$country)
  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # LC 1                                                      #
  # --------------------------------------------------------- #
  output$lc1table <- {
    tmp7 <- copy(lc1info)
    setorder(tmp7, -itemcount)
    tmp7[, controlled_circ:=sprintf("%.2f", controlled_circ)]
    tmp7[, percent_coll:=sprintf("%.2f%%", 100*percent_coll)]
    tmp7[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp7[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp7[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp7[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp7[, fy19_circ:=prettyNum(fy19_circ, big.mark=",")]
    tmp7[, fy20_circ:=prettyNum(fy20_circ, big.mark=",")]
    tmp7[, fy21_circ:=prettyNum(fy21_circ, big.mark=",")]
    tmp7[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp7, "itemcount", "Item Count")
    setnames(tmp7, "bibcount", "Bib Count")
    setnames(tmp7, "date.div", "Date Spread")
    setnames(tmp7, "fy17_circ", "FY17 Circ")
    setnames(tmp7, "fy18_circ", "FY18 Circ")
    setnames(tmp7, "fy19_circ", "FY19 Circ")
    setnames(tmp7, "fy20_circ", "FY20 Circ")
    setnames(tmp7, "fy21_circ", "FY21 Circ")
    setnames(tmp7, "total_circ", "Total Circ")
    setnames(tmp7, "controlled_circ", "Circ Per Item")
    setnames(tmp7, "percent_coll", "Percent of collection")
    setnames(tmp7, "first_letter", "Letter")
    setnames(tmp7, "lc_subject_class", "LC Subject Class")
    tmp7[, short_desc:=NULL]
    renderDT(tmp7)
  }

  output$downloadlc1rawdata <- downloadHandler(
    filename = "raw-lc1-data.txt",
    content = function(file){
      fwrite(lc1info, file, sep="\t")
    }
  )

  output$lc1pie <- renderPlot({
    setorder(lc1info, -itemcount)
    tmp <- compress(lc1info[, .(short_desc, itemcount)], input$pienumlc1)
    pie(tmp$itemcount, labels=tmp$short_desc)
  })

  output$lc1explorerbar <- renderPlot({
    tmpmat <- as.matrix(lc1norm[, 3:6])
    coeffs <- c(input$lc1explore_numitems,
                input$lc1explore_datediv,
                input$lc1explore_rawcirc,
                input$lc1explore_controlledcirc)
    coeffmat <- matrix(coeffs, ncol=1)
    tmp <- tmpmat %*% coeffmat
    res <- data.table(description=lc1norm[, short_desc],
                      score=as.vector(tmp))
    setorder(res, -score)
    res <- res[1:input$lc1explorer_limit,]
    barplot(res[, score], names.arg=res[, description], yaxt="n")
  })
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # LC 2                                                      #
  # --------------------------------------------------------- #
  output$lc2table <- {
    tmp8 <- copy(lc2info)
    setorder(tmp8, -itemcount)
    tmp8[, controlled_circ:=sprintf("%.2f", controlled_circ)]
    tmp8[, percent_coll:=sprintf("%.2f%%", 100*percent_coll)]
    tmp8[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp8[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp8[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp8[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp8[, fy19_circ:=prettyNum(fy19_circ, big.mark=",")]
    tmp8[, fy20_circ:=prettyNum(fy20_circ, big.mark=",")]
    tmp8[, fy21_circ:=prettyNum(fy21_circ, big.mark=",")]
    tmp8[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp8, "itemcount", "Item Count")
    setnames(tmp8, "bibcount", "Bib Count")
    setnames(tmp8, "date.div", "Date Spread")
    setnames(tmp8, "fy17_circ", "FY17 Circ")
    setnames(tmp8, "fy18_circ", "FY18 Circ")
    setnames(tmp8, "fy19_circ", "FY19 Circ")
    setnames(tmp8, "fy20_circ", "FY20 Circ")
    setnames(tmp8, "fy21_circ", "FY21 Circ")
    setnames(tmp8, "total_circ", "Total Circ")
    setnames(tmp8, "controlled_circ", "Circ Per Item")
    setnames(tmp8, "percent_coll", "Percent of collection")
    setnames(tmp8, "all_letters", "Letters")
    setnames(tmp8, "lc_subject_subclass", "LC Subject Subclass")
    renderDT(tmp8)
  }

  output$downloadlc2rawdata <- downloadHandler(
    filename = "raw-lc2-data.txt",
    content = function(file){
      fwrite(lc2info, file, sep="\t")
    }
  )
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # Scan and deliver                                          #
  # --------------------------------------------------------- #
  output$sanddreqsplot <- renderPlot({
    tmpdata <- sandddaily
    if(input$sanddfreqopt=="Weekly")
      tmpdata <- sanddweekly
    tmpfn <- ifelse(input$sanddsmoothp=="Yes", geom_smooth, geom_line)
    tmpxlab <- ifelse(input$sanddfreqopt=="Weekly", "week", "date")
    ggplot(tmpdata[center==input$sanddcenteropt], aes(x=thetime, y=total)) +
      tmpfn(size=1.4, color="blue", alpha=0.6) +
      xlab(tmpxlab)
  })

  output$sanddlangtable <- {
    tmp10 <- copy(sanddlang)
    tmp10 <- tmp10[lang!="TOTAL"]
    setnames(tmp10, "lang", "language")
    setorder(tmp10, -count)
    renderDT(tmp10)
  }

  output$downloadsanddlangrawdata <- downloadHandler(
    filename = "raw-scan-and-deliver-lang-data.txt",
    content = function(file){
      fwrite(sanddlang, file, sep="\t")
    }
  )

  output$sanddlc1table <- {
    tmp11 <- copy(sanddlc1)
    tmp11 <- tmp11[subject_classification!="TOTAL"]
    setorder(tmp11, -count)
    renderDT(tmp11)
  }

  output$downloadsanddlc1rawdata <- downloadHandler(
    filename = "raw-scan-and-deliver-lc1-data.txt",
    content = function(file){
      fwrite(sanddlc1, file, sep="\t")
    }
  )
  # --------------------------------------------------------- #


  # --------------------------------------------------------- #
  # EZ Proxy
  # --------------------------------------------------------- #

  output$ezuniqsessionsplot <- renderPlot({
    toplimit <- input$eztopvendors
    lowerbound <- ifelse(input$eztotalp=="Yes", 0, 1)
    tmpfn <- ifelse(input$ezsmoothp=="Yes",
                    function(x){geom_smooth(method="gam")},
                    geom_line)
    ggplot(ezprox1[venrank<=toplimit & venrank>=lowerbound],
           aes(x=just_date, y=unique_sessions, group=vendor,
               color=vendor, fill=vendor)) +
      tmpfn() +
      xlab("date") + ylab("number of unique sessions")
  })

  # --------------------------------------------------------- #
}




shinyApp(ui, server)




