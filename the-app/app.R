#!/usr/local/bin//Rscript --vanilla


library(shiny)
library(shinydashboard)
library(data.table)
library(stringr)
library(DT)
library(ggplot2)
library(forecast)



options(warn=1)
options(scipen=20)

#####################################
# set up

nicecenterinfo <- fread("./data/nice-wmr-and-lair-quarterly-stats.txt", sep="\t", header=TRUE)
setorder(nicecenterinfo, FY, quarter)
nicecenterinfo[, period:=nicecenterinfo[, sprintf("FY%s-Q%d", as.character(FY), quarter)]]
nicetotals <- nicecenterinfo[, .(totals=sum(circ, na.rm=TRUE)), .(FY, quarter)]
niceyeartotals <- nicetotals[, sum(totals), FY]
visitinfo <- fread("./data/visits-by-quarter.dat", sep="\t", header=TRUE)

geninfo <- fread("./data/gen-info.dat", sep="\t", header=TRUE)
langinfo <- fread("./data/langinfo.dat", sep="\t", header=TRUE)
langnorm <- fread("./data/langnorm.dat", sep="\t", header=TRUE)
locationinfo <- fread("./data/centerinfo.dat", sep="\t", header=TRUE)
mattypeinfo <- fread("./data/xmattype.dat", sep="\t", header=TRUE)
biblevelinfo <- fread("./data/xbiblevel.dat", sep="\t", header=TRUE)
countriesinfo  <- fread("./data/countryinfo.dat", sep="\t", header=TRUE)
locationinfo <- fread("./data/centerinfo.dat", sep="\t", header=TRUE)
lc1info <- fread("./data/lc1-info.dat", sep="\t", header=TRUE)
lc1info[, short_desc:=ifelse(str_length(lc1info[, description])>20,
                             sprintf("(%s) %s...",
                                     lc1info[, first_letter],
                                     str_sub(lc1info[, description], 1, 17)),
                             sprintf("(%s) %s",
                                     lc1info[, first_letter],
                                     lc1info[, description]))]
lc1norm <- fread("./data/lc1norm.dat", sep="\t", header=TRUE)
lc1norm[, short_desc:=ifelse(str_length(lc1norm[, description])>20,
                             sprintf("(%s) %s...",
                                     lc1norm[, first_letter],
                                     str_sub(lc1norm[, description], 1, 17)),
                             sprintf("(%s) %s",
                                     lc1norm[, first_letter],
                                     lc1norm[, description]))]
lc2info <- fread("./data/lc2-info.dat", sep="\t", header=TRUE)
lc2norm <- fread("./data/lc2norm.dat", sep="\t", header=TRUE)

edddaily  <- fread("./data/edd-daily.dat", sep='\t', header=TRUE)
eddweekly <- fread("./data/edd-weekly.dat", sep='\t', header=TRUE)
setnames(edddaily, "xdate", "thetime")
# setnames(eddweekly, "theweek", "thetime")
setnames(eddweekly, "xdate", "thetime")

#ALL <- fread("./data/")

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


make.matrix.result <- function(thelabels, themat, coeffs){
  coeffmat <- matrix(coeffs, ncol=1)
  res <- themat %*% coeffmat
  return(data.table(thelabel=thelabels,
                    results=res))
}


header <- dashboardHeader(
  title = "NYPL Research",
  dropdownMenu(type = "notifications",
               notificationItem(
                 text="See footnotes at bottom of main tab",
                 icon=icon("exclamation-triangle"),
                 status="warning")),
  dropdownMenu(type = "tasks",
               taskItem(value=80, color="green",
                        HTML("Finish automation of LC Call<br> Number cross-pollination")
                        ),
               taskItem(value=50, color="yellow",
                        HTML("Make ReCAP numbers not hard-coded")
                        ),
               taskItem(value=10, color="red",
                        HTML("Transfer this dashboard to a new domain")
                        )
              )

)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),

    menuItem("Research Centers", tabName = "location", icon = icon("r-project"),
             menuSubItem("General info", tabName="locationgen", icon=icon("pie-chart"))),
    
    menuItem("Circulation", tabName = "circsuper", icon = icon("bezier-curve"),
             menuSubItem("Circ Overview", tabName="circsuboverview", icon=icon("dashboard")),
             menuSubItem("Overall quarterly circ", tabName="circoverallview", icon=icon("line-chart")),
             menuSubItem("Circ by center", tabName="circbycenter", icon=icon("line-chart")),
             menuSubItem(icon=NULL,
               selectInput("centeropt", "Center:",
                                     c("SASB" = "SASB",
                                       "Schomburg" = "Schomburg",
                                       "LPA" = "LPA",
                                       "SIBL" = "SIBL"
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
    
    menuItem("LC subject (I)", tabName = "lc1super", icon = icon("wheelchair"),
             menuSubItem("Raw data table", tabName="lc1subraw", icon=icon("table")),
             menuSubItem("Pie chart", tabName="lc1subpie", icon=icon("pie-chart")),
             menuSubItem("Explorer", tabName="lc1subexplorer", icon=icon("search-plus"))
    ),
    
    menuItem("EDD", tabName = "eddrtab", icon = icon("bolt"),
             menuSubItem("Scan and deliver", tabName="sanddtab", icon=icon("truck")),
             menuSubItem(icon=NULL,
                         selectInput("sanddcenteropt", "Center:",
                                     c("SASB" = "SASB",
                                       "Schomburg" = "Schomburg",
                                       "LPA" = "LPA",
                                       "All Centers" = "total"
                                     ))
             ),
             menuSubItem(icon=NULL,
                         selectInput("sanddfreqopt", "Frequency:",
                                     c("Daily" = "Daily",
                                       "Weekly" = "Weekly"
                                     ))
             ),
             menuSubItem(icon=NULL,
                         selectInput("sanddsmoothp", "Loess smoothing?:",
                                     c("None" = "None",
                                       "Yes" = "Yes"
                                     ))
             )
    ),
    
    menuItem("Aeon", tabName = "aeontab", icon = icon("palette"),
             menuSubItem("coming soon", tabName="garbage", icon=icon("table"))
    )
    
  )
)

body <- dashboardBody(
  tabItems(

    # OVERVIEW TAB
    tabItem(tabName = "overview",
            h1("Overview"),
            br(),
            fluidRow(
              valueBoxOutput("totalItemsValueBox"),
              valueBoxOutput("totalBibsValueBox")
            ),
            br(),
            fluidRow(
              valueBoxOutput("recapNewItemsValueBox"),
              valueBoxOutput("recapNewBibsValueBox")
            ),
            br(),
            fluidRow(
              valueBoxOutput("fy18checkoutsValueBox"),
              valueBoxOutput("fy19checkoutsValueBox"),
              valueBoxOutput("fy20checkoutsValueBox")
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                          "*    based on a Sierra database export performed on 2020-05. Circulations are only for barcoded Sierra items and only reflect circulations since Sierra/Millenium",
                          br(),
                          "*    does not include shared collection items from other institutions",
                          br(),
                          br(),
                          "†    does not include new Harvard integration candidates (potentially 3.8 million titles)",
                          br(),
                          br(),
                          "‡    This includes all Sierra checkouts (general collections) and special collection usage as reported on the Web Management Report and the Cloud Apps Portal, respectively",
                          width=7)))
    ),

    # LOCATION TAB
    tabItem(tabName = "locationgen",
            h1("Research Centers"),
            downloadButton("downloadcenterrawdata", "Download"),
            br(),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Tabular info",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       DTOutput('centertable'),
                       width=12
                     )
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
                       plotOutput("locationpie"), width=12)
              )
            )
    ),
    
    tabItem(tabName = "circsuboverview",
            h1("Circulation overview"),
            br(),
            fluidRow(
              valueBoxOutput("fy18checkoutsValueBox2"),
              valueBoxOutput("fy19checkoutsValueBox2"),
              valueBoxOutput("fy20checkoutsValueBox2")
            ),
            br(), br(), br(), br(),
            br(), br(), br(), br(),
            fluidRow(
              column(12,
                     box(title="footnotes",
                         collapsible=TRUE,
                         "‡    This includes all Sierra checkouts (general collections) and special collection usage as reported on the Web Management Report and the Cloud Apps Portal, respectively",
                         width=7)))
    ),

    tabItem(tabName = "circoverallview",
            h1("Overall Quarterly Circulation"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Overall circ by quarter",
                       collapsible = TRUE,
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("totalquarterlycircs"), width=12))),
    ),
    

    tabItem(tabName = "circbycenter",
            h1("Circulation by research center"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Quarterly circulation by center",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("quarterlycircbycenter"), width=12)
              )
            )
    ),

    
    tabItem(tabName = "visitssuboverview",
            h1("Visits overview"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Visits by quarter",
                       collapsible = TRUE,
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("totalquarterlyvisits"), width=12)
              )
            )
    ),



    #  BIB LEVEL TAB
    tabItem(tabName = "biblevelsubraw",
            h1("Bib Level"),
            downloadButton("downloadbiblevelrawdata", "Download"),
            br(),
            br(),
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
            )
    ),

    tabItem(tabName = "biblevelsubpie",
            h1("Bib Level"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items at each bib level",
                       solidHeader = TRUE,
                       status="primary",
                       plotOutput("biblevelpie"), width=9),
                     box(
                       title = "Controls",
                       solidHeader = TRUE,
                       status="primary",
                       sliderInput("pienumbiblevel", "Number of top bib level:", 2, 8, 3),
                       width=3)
              )
            )
    ),



    #  MATERIAL TYPE TAB
    tabItem(tabName = "mattypesubraw",
            h1("Material Type"),
            downloadButton("downloadmattyperawdata", "Download"),
            br(),
            br(),
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
            )
    ),

    tabItem(tabName = "mattypesubpie",
            h1("Material Type"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each material type catagory",
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


    # LANGUAGE TAB
    tabItem(tabName = "languagesubraw",
            h1("Language"),
            downloadButton("downloadlangrawdata", "Download"),
            br(),
            br(),
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
            )
    ),

    tabItem(tabName = "languagesubpie",
            h1("Language"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each language",
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




    # PLACE OF PUBLICATION TAB
    tabItem(tabName = "pubplacesubraw",
            h1("Place of publication"),
            downloadButton("downloadpubplacerawdata", "Download"),
            br(),
            br(),
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
            )
    ),

    tabItem(tabName = "pubplacesubpie",
            h1("Place of publication"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items published in place",
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
    
    
    tabItem(tabName = "sanddtab",
            h1("Scan and deliver"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of filled requests (from Sierra)",
                       solidHeader = TRUE,
                       status="primary",
                       collapsible = TRUE,
                       plotOutput("sanddplot"), width=12)
              )
            )
    ),




    # LC1 TAB
    tabItem(tabName = "lc1subraw",
            h1("Library of Congress subject categories (broad)"),
            downloadButton("downloadlc1rawdata", "Download"),
            br(),
            br(),
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
            )
    ),

    tabItem(tabName = "lc1subpie",
            h1("Library of Congress subject categories (broad)"),
            br(),
            fluidRow(
              column(12,
                     box(
                       title="Number of items in each subject category",
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
            h1("Library of Congress subject categories (broad) [explorer]"),
            br(),
            fluidRow(
              column(width=9,
                     box(
                       title="Strength explorer (play with the strength coefficients)",
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
    )










  )
)








ui <- dashboardPage(header, sidebar, body)





server <- function(input, output) {



  # --------------------------------------------------------- #
  # OVERVIEW                                                  #
  # --------------------------------------------------------- #
  output$totalItemsValueBox <- renderValueBox({
    valueBox(
      prettyNum(as.integer(geninfo[4, 3]), big.mark=","),
      "Total barcoded items in collection*",
      color="purple",
      icon=icon("book")
    )
  })

  output$totalBibsValueBox <- renderValueBox({
    valueBox(
      prettyNum(as.integer(geninfo[3, 3]), big.mark=","),
      "Total number of titles in collection*",
      color="purple",
      icon=icon("book")
    )
  })
  
  output$recapNewItemsValueBox <- renderValueBox({
    valueBox(
      prettyNum(7876464, big.mark=","),
      "New accessible non-NYPL items through Shared Collection†",
      color="red",
      icon=icon("exchange")
    )
  })

  output$recapNewBibsValueBox <- renderValueBox({
    valueBox(
      prettyNum(5496959, big.mark=","),
      "New accessible non-NYPL titles through Shared Collection†",
      color="red",
      icon=icon("exchange")
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
    # tmp <- paulcenterinfo[, .SD, .SDcols=names(paulcenterinfo) %like% sprintf("^%s|period", theone)]
    # tmp <- melt(tmp, id.vars="period")
    # tmp <- tmp[!is.na(value)]
    # tmp[, metric:=str_replace_all(str_replace(variable, sprintf("%s_", theone), ""), "_", " ")]
    # ggplot(tmp, aes(x=period, y=value, group=metric, fill=metric, color=metric)) +
    #   geom_line() +
    #   ggtitle(sprintf("%s circulation and materials consulted", theone))
    tmp <- nicecenterinfo[center==theone]
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
    tmp[, controlled_circ:=sprintf("%.3f", controlled_circ)]
    tmp[, percent_coll:=sprintf("%.3f%%", 100*percent_coll)]
    tmp[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp, "itemcount", "Item Count")
    setnames(tmp, "bibcount", "Bib Count")
    setnames(tmp, "date.div", "Date Spread")
    setnames(tmp, "fy17_circ", "FY17 Circ")
    setnames(tmp, "fy18_circ", "FY18 Circ")
    setnames(tmp, "total_circ", "Total Circ")
    setnames(tmp, "controlled_circ", "Circ Per Item")
    setnames(tmp, "percent_coll", "Percent of collection")
    renderDT(tmp,
             options=list(
    #           initComplete = JS("
    #function(settings, json) {
    #  $(this.api().table().header()).css({
    #    'background-color': '#000',
    #    'color': '#fff'
    #  });
    #}")
             ))
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
  # COUNTRIES                                                 #
  # --------------------------------------------------------- #
  output$countriestable <- {
    tmp8 <- copy(countriesinfo)
    setorder(countriesinfo, -itemcount)
    setorder(tmp8, -itemcount)
    tmp8[, controlled_circ:=sprintf("%.3f", controlled_circ)]
    tmp8[, percent_coll:=sprintf("%.3f%%", 100*percent_coll)]
    tmp8[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp8[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp8[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp8[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp8[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp8, "itemcount", "Item Count")
    setnames(tmp8, "bibcount", "Bib Count")
    setnames(tmp8, "date.div", "Date Spread")
    setnames(tmp8, "fy17_circ", "FY17 Circ")
    setnames(tmp8, "fy18_circ", "FY18 Circ")
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
  # EDD                                                       #
  # --------------------------------------------------------- #
  
  output$sanddplot <- renderPlot({
    
    tmpdata <- edddaily
    if(input$sanddfreqopt=="Weekly")
      tmpdata <- eddweekly
    tmpfn <- ifelse(input$sanddsmoothp=="Yes", geom_smooth, geom_line)
    tmpxlab <- ifelse(input$sanddfreqopt=="Weekly", "week", "date")
    
    ggplot(tmpdata[center==input$sanddcenteropt], aes(x=thetime, y=total)) +
      tmpfn(size=1.4, color="blue", alpha=0.6) +
      xlab(tmpxlab)
  })
  # --------------------------------------------------------- #
  



  # --------------------------------------------------------- #
  # LC 1                                                      #
  # --------------------------------------------------------- #
  output$lc1table <- {
    tmp7 <- copy(lc1info)
    #print(names(tmp7))
    setorder(lc1info, -itemcount)
    setorder(tmp7, -itemcount)
    tmp7[, controlled_circ:=sprintf("%.3f", controlled_circ)]
    tmp7[, percent_coll:=sprintf("%.3f%%", 100*percent_coll)]
    tmp7[, itemcount:=prettyNum(itemcount, big.mark=",")]
    tmp7[, bibcount:=prettyNum(bibcount, big.mark=",")]
    tmp7[, fy17_circ:=prettyNum(fy17_circ, big.mark=",")]
    tmp7[, fy18_circ:=prettyNum(fy18_circ, big.mark=",")]
    tmp7[, total_circ:=prettyNum(total_circ, big.mark=",")]
    setnames(tmp7, "itemcount", "Item Count")
    setnames(tmp7, "bibcount", "Bib Count")
    setnames(tmp7, "date.div", "Date Spread")
    setnames(tmp7, "fy17_circ", "FY17 Circ")
    setnames(tmp7, "fy18_circ", "FY18 Circ")
    setnames(tmp7, "total_circ", "Total Circ")
    setnames(tmp7, "controlled_circ", "Circ Per Item")
    setnames(tmp7, "percent_coll", "Percent of collection")
    setnames(tmp7, "first_letter", "Letter")
    setnames(tmp7, "description", "Subject")
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







}





shinyApp(ui, server)




