#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(datatable.prettyprint.char=50)
options(width = 80)

args <- commandArgs(trailingOnly=TRUE)

library(colorout)
library(data.table)
library(magrittr)
library(stringr)
library(BBmisc)

source("~/.rix/tony-utils.R")
# ------------------------------ #


dat <- readRDS("../sierra-research-healed-joined.datatable")

justincase <- copy(dat)

dat[, .N]
# 2020-07-28: 10,895,556
# ?         : 10,604,751

# fix dates (will upstream this [TODO])
dat[!is.na(pub_year) & pub_year > 2020, pub_year:=NA]
dat[!is.na(pub_year) & pub_year < 1000, .(pub_year)]
dat[!is.na(pub_year) & pub_year < 170, pub_year:=NA]
dat[!is.na(pub_year) & pub_year==999, pub_year:=NA]
dat[!is.na(pub_year) & pub_year < 1000 & pub_year > 201, pub_year:=NA]
dat[!is.na(pub_year) & pub_year < 1000, pub_year:=as.integer(10*pub_year)]




# --------------------------------------------------------------- #
# -----------               LANGUAGE            ------------------#
# --------------------------------------------------------------- #


setkey(dat, "langcode")

dat[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(langcode, lang)]                               -> langinfo


langinfo[!is.na(langcode)]                          -> langinfo

langinfo[, itemcount] %>% hist

# ONLY LANGUAGES WITH MORE THAN 50 ITEMS!
langinfo[itemcount>50]                              -> langinfo

langinfo[, controlled_circ:=total_circ/itemcount]   -> langinfo
langinfo[, percent_coll:=itemcount/sum(itemcount)]


setnames(langinfo, "lang", "language")
langinfo %>% fwrite("./langinfo.dat", sep="\t")

# normalization

langnorm <- copy(langinfo)

langnorm[, itemcount:=normalize(itemcount, method="range")]
langnorm[, date.div:=normalize(date.div, method="range")]
langnorm[, total_circ:=normalize(total_circ, method="range")]
langnorm[, controlled_circ:=normalize(controlled_circ, method="range")]
langnorm

keepcols(langnorm, c("langcode", "language", "itemcount",
                     "date.div", "total_circ", "controlled_circ"))


langnorm %>% fwrite("./langnorm.dat", sep="\t")


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #
# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
# -----------              LC SUBJECT            -----------------#
# --------------------------------------------------------------- #

load("../../sysdata.rda")
source("./lccall-subject.R")

dat[, .N]

dat[, lc_p:=get_lc_subject_letters(lccall)]

dat[, .N, is.na(lc_p)]
# is.na       N
# <lgcl>   <int>
# 1:   TRUE 6309247
# 2:  FALSE 4586309
#   OLD
    #     is.na       N
    #    <lgcl>   <int>
    # 1:   TRUE 6243044
    # 2:  FALSE 4361707


dat[!is.na(lc_p), ]                           -> tmp

secondlevelxwalk  <- readRDS("../../second-level.datatable")
broadxwalk        <- readRDS("../../first-letter.datatable")
setnames(tmp, "lc_p", "one_below")

tmp[, one_below:=toupper(one_below)]
tmp[, first_letter:=substr(one_below, 1, 1)]


setkey(tmp, "first_letter")


tmp[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   first_letter]                              -> lc1info

lc1info

lc1info[, controlled_circ:=total_circ/itemcount]
lc1info[, percent_coll:=itemcount/sum(itemcount)]

broadxwalk[lc1info]                           -> lc1info

lc1info %>% fwrite("./lc1-info.dat", sep="\t")


# normalization

lc1norm <- copy(lc1info)

lc1norm[, itemcount:=normalize(itemcount, method="range")]
lc1norm[, date.div:=normalize(date.div, method="range")]
lc1norm[, total_circ:=normalize(total_circ, method="range")]
lc1norm[, controlled_circ:=normalize(controlled_circ, method="range")]
lc1norm

keepcols(lc1norm,  c("first_letter", "description", "itemcount",
                     "date.div", "total_circ", "controlled_circ"))

lc1norm %>% fwrite("./lc1norm.dat", sep="\t")


### lc2 now

setkey(tmp, "one_below")
tmp[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   one_below]                              -> lc2info

lc2info[, controlled_circ:=total_circ/itemcount]
lc2info[, percent_coll:=itemcount/sum(itemcount)]

secondlevelxwalk[lc2info]                 -> lc2info

lc2info %>% fwrite("./lc2-info.dat", sep="\t")


# normalization

lc2norm <- copy(lc2info)

lc2norm[, itemcount:=normalize(itemcount, method="range")]
lc2norm[, date.div:=normalize(date.div, method="range")]
lc2norm[, total_circ:=normalize(total_circ, method="range")]
lc2norm[, controlled_circ:=normalize(controlled_circ, method="range")]
lc2norm

keepcols(lc2norm,  c("one_below", "description", "itemcount",
                     "date.div", "total_circ", "controlled_circ"))
lc2norm %>% fwrite("./lc2norm.dat", sep="\t")





# --------------------------------------------------------------- #
# -----------               MAT TYPE             -----------------#
# --------------------------------------------------------------- #

dat[, .(itype, biblevel, mattype)]

dat[, .N, itype]
dat[, .N, biblevel]
dat[, .N, mattype]
dat[, .N, .(biblevel, mattype)]

dat[, .N, biblevel][!is.na(biblevel) & biblevel!="---"] -> xbiblevel
xbiblevel[, percent_coll:=N/sum(N)]
xbiblevel
xbiblevel %>% fwrite("./xbiblevel.dat", sep="\t")


dat[, .N, mattype][N>1000] -> xmattype
xmattype[, percent_coll:=N/sum(N)]
xmattype
xmattype %>% fwrite("./xmattype.dat", sep="\t")






rm(tmp)

# --------------------------------------------------------------- #
# -----------                COUNTRY             -----------------#
# --------------------------------------------------------------- #


setkey(dat, "countrycode")

dat[, .N, .(countrycode)]

dat[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(countrycode, country)]                    -> countryinfo

delcols(countryinfo, "countrycode")

countryinfo[, .(itemcount=sum(itemcount),
                bibcount=sum(bibcount),
                date.div=mean(date.div),
                fy17_circ=sum(fy17_circ),
                fy18_circ=sum(fy18_circ),
                fy19_circ=sum(fy19_circ),
                fy20_circ=sum(fy20_circ),
                total_circ=sum(total_circ)),
  country]                                    -> countryinfo

countryinfo[order(-itemcount)]

countryinfo[, controlled_circ:=total_circ/itemcount]
countryinfo[controlled_circ>2, controlled_circ:=.94]    # HACK
countryinfo[, percent_coll:=itemcount/sum(itemcount)]

countryinfo %>% fwrite("./countryinfo.dat", sep="\t")



# normalization

countrynorm <- copy(countryinfo)

countrynorm[, itemcount:=normalize(itemcount, method="range")]
countrynorm[, date.div:=normalize(date.div, method="range")]
countrynorm[, total_circ:=normalize(total_circ, method="range")]
countrynorm[, controlled_circ:=normalize(controlled_circ, method="range")]
countrynorm

keepcols(countrynorm,  c("country", "description", "itemcount",
                     "date.div", "total_circ", "controlled_circ"))
countrynorm %>% fwrite("./countrynorm.dat", sep="\t")





# --------------------------------------------------------------- #
# -----------            CENTER/LOCATION         -----------------#
# -----------            CENTER/LOCATION         -----------------#
# --------------------------------------------------------------- #

locxwalk <- readRDS("../../locationxwalk.datatable")

setkey(dat, "item_location_code")

dat[, .(num_items=uniqueN(itemid),
        num_bibs =uniqueN(bibid),
        total_circ=sum(total_circ, na.rm=TRUE)),
  item_location_code]                 -> tmp

setnames(tmp, "item_location_code", "location_code")
setkey(tmp, "location_code")


locxwalk[tmp]                         -> tmp

tmp[!is.na(center)]                   -> tmp


tmp[, .(num_items =sum(num_items),
        num_bibs  =sum(num_bibs),
        total_circ=sum(total_circ)),
  center]                             -> centerinfo

centerinfo[, percent_coll:=num_items/sum(num_items)]
centerinfo[, controlled_circ:=total_circ/num_items]

centerinfo %>% fwrite("./centerinfo.dat", sep="\t")




# --------------------------------------------------------------- #
# -----------              GENERAL INFO!         -----------------#
# -----------              GENERAL INFO!         -----------------#
# --------------------------------------------------------------- #


build.a.count <- data.table(dacat="",
                            dakey="",
                            davalue="",
                            dadesc="")

build.row <- function(acat, akey, aval, adesc){
  data.table(dacat=acat, dakey=akey, davalue=aval, dadesc=adesc)
}

add.to.build.a.count <- function(acat, akey, aval, adesc){
  tmp <- build.row(acat, akey, aval, adesc)
  build.a.count <<- rbind(build.a.count, tmp)
}


add.to.build.a.count("gen_info", "num_uniq_bibs_and_items",
                     sprintf("%d", uniqueN(dat[, .(bibid, itemid)])),
                     "Number of unique bibid and itemid pairs")

add.to.build.a.count("gen_info", "num_uniq_bibs",
                     sprintf("%d", uniqueN(dat[, .(bibid)])),
                     "Number of unique bibs")

add.to.build.a.count("gen_info", "num_uniq_items",
                     sprintf("%d", uniqueN(dat[, .(itemid)])),
                     "Number of unique items")

add.to.build.a.count("gen_info", "grand_total_circ",
                     sprintf("%d", dat[, sum(total_circ, na.rm=TRUE)]),
                     "Grand total circulation")

add.to.build.a.count("gen_info", "perc_circed_once",
                     sprintf("%.2f%%", 100*dat[total_circ>0, .N]/dat[,.N]),
                     "Percent of collection that circed at least once")

add.to.build.a.count("gen_info", "fy17circ",
                     sprintf("%d", dat[, sum(fy17_checkouts, na.rm=TRUE)]),
                     "Total check-outs in FY17")

add.to.build.a.count("gen_info", "fy18circ",
                     sprintf("%d", dat[, sum(fy18_checkouts, na.rm=TRUE)]),
                     "Total check-outs in FY18")

add.to.build.a.count("gen_info", "fy19circ",
                     sprintf("%d", dat[, sum(fy19_checkouts, na.rm=TRUE)]),
                     "Total check-outs in FY19")

add.to.build.a.count("gen_info", "fy20circ",
                     sprintf("%d", dat[, sum(fy20_checkouts, na.rm=TRUE)]),
                     "Total check-outs in FY20")


build.a.count %>% fwrite("./gen-info.dat", sep="\t")






# --------------------------------------------------------------- #
# -----------           ALL DATA (EXCERPT)       -----------------#
# -----------           ALL DATA (EXCERPT)       -----------------#
# --------------------------------------------------------------- #

short <- copy(dat)







