#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(width=80)
options(warn=2)
options(scipen=10)
options(datatable.prettyprint.char=50)
options(datatable.print.class=TRUE)
options(datatable.print.keys=TRUE)
options(datatable.fwrite.sep='\t')
options(datatable.na.strings="")

args <- commandArgs(trailingOnly=TRUE)

library(colorout)
library(data.table)
library(magrittr)
library(stringr)
library(libbib)     # >= v1.6.2
library(BBmisc)
library(assertr)
# ------------------------------ #


SHADOW_SIERRA_LOCATION  <- "~/Dropbox/NYPL/nypl-shadow-export/target/"


# ------------------------------ #


dat <- fread_plus_date(sprintf("%s/sierra-research-healed-joined.dat.gz",
                               SHADOW_SIERRA_LOCATION))
set_lb_attribute(dat, "source", "sierra shadow database")
set_lb_attribute(dat, "note", "derived from data substrate from https://github.com/NYPL/sierra-shadow-dataset")


justincase <- copy(dat)

dat[, .N]
# dat %>% verify(nrow(.) >= 10895556, success_fun=success_report) # 2020-07-28
dat %>% verify(nrow(.) >= 10888765, success_fun=success_report) # 2021-04-08



###################################################################
###################################################################
### METRICS FROM SIERRA
###################################################################


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
        fy21_circ=sum(fy21_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(langcode, lang)]                               -> langinfo


langinfo[!is.na(langcode)]                          -> langinfo

# ONLY LANGUAGES WITH MORE THAN 50 ITEMS!
langinfo[itemcount>50]                              -> langinfo

langinfo[, controlled_circ:=total_circ/itemcount]   -> langinfo
langinfo[, percent_coll:=itemcount/sum(itemcount)]


setnames(langinfo, "lang", "language")
cp_lb_attributes(dat, langinfo)
langinfo %>% fwrite_plus_date("./target/langinfo.dat")

# normalization

langnorm <- copy(langinfo)

langnorm[, itemcount:=normalize(itemcount, method="range")]
langnorm[, date.div:=normalize(date.div, method="range")]
langnorm[, total_circ:=normalize(total_circ, method="range")]
langnorm[, controlled_circ:=normalize(controlled_circ, method="range")]
langnorm

dt_keep_cols(langnorm, c("langcode", "language", "itemcount",
                         "date.div", "total_circ", "controlled_circ"))


langnorm %>% fwrite_plus_date("./target/langnorm.dat")


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #
# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
# -----------              LC SUBJECT            -----------------#
# --------------------------------------------------------------- #


dat[!is.na(lc_subject_class), first_letter:=get_lc_call_first_letter(lccall, allow.bare=TRUE)]

dat[!is.na(lc_subject_class), ] -> tmp

setkey(tmp, "first_letter")


tmp[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        fy21_circ=sum(fy21_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(first_letter, lc_subject_class)]                 -> lc1info

lc1info

lc1info[, controlled_circ:=total_circ/itemcount]
lc1info[, percent_coll:=itemcount/sum(itemcount)]

cp_lb_attributes(dat, lc1info)
lc1info %>% fwrite_plus_date("./target/lc1-info.dat")


# normalization

lc1norm <- copy(lc1info)

lc1norm[, itemcount:=normalize(itemcount, method="range")]
lc1norm[, date.div:=normalize(date.div, method="range")]
lc1norm[, total_circ:=normalize(total_circ, method="range")]
lc1norm[, controlled_circ:=normalize(controlled_circ, method="range")]
lc1norm

dt_keep_cols(lc1norm,  c("first_letter", "lc_subject_class", "itemcount",
                         "date.div", "total_circ", "controlled_circ"))

lc1norm %>% fwrite_plus_date("./target/lc1-norm.dat")


### lc2 now

dat[!is.na(lc_subject_subclass), all_letters:=get_all_lc_call_subject_letters(lccall, allow.bare=TRUE)]

dat[!is.na(lc_subject_subclass), ] -> tmp


setkey(tmp, "all_letters")

tmp[, .(itemcount=.N,
        bibcount=uniqueN(bibid),
        date.div=mad(pub_year, na.rm=TRUE),
        fy17_circ=sum(fy17_checkouts, na.rm=TRUE),
        fy18_circ=sum(fy18_checkouts, na.rm=TRUE),
        fy19_circ=sum(fy19_checkouts, na.rm=TRUE),
        fy20_circ=sum(fy20_checkouts, na.rm=TRUE),
        fy21_circ=sum(fy21_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(all_letters, lc_subject_subclass)]                   -> lc2info


lc2info[, controlled_circ:=total_circ/itemcount]
lc2info[, percent_coll:=itemcount/sum(itemcount)]

cp_lb_attributes(dat, lc2info)
lc2info %>% fwrite_plus_date("./target/lc2-info.dat")


# normalization

lc2norm <- copy(lc2info)

lc2norm[, itemcount:=normalize(itemcount, method="range")]
lc2norm[, date.div:=normalize(date.div, method="range")]
lc2norm[, total_circ:=normalize(total_circ, method="range")]
lc2norm[, controlled_circ:=normalize(controlled_circ, method="range")]
lc2norm

dt_keep_cols(lc2norm,  c("all_letters", "lc_subject_subclass", "itemcount",
                         "date.div", "total_circ", "controlled_circ"))
lc2norm %>% fwrite_plus_date("./target/lc2-norm.dat", sep="\t")





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
cp_lb_attributes(dat, xbiblevel)
xbiblevel %>% fwrite_plus_date("./target/xbiblevel.dat")


dat[, .N, mattype][N>100] -> xmattype
xmattype[, percent_coll:=N/sum(N)]
xmattype
cp_lb_attributes(dat, xmattype)
xmattype %>% fwrite_plus_date("./target/xmattype.dat")



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
        fy21_circ=sum(fy21_checkouts, na.rm=TRUE),
        total_circ=sum(total_circ, na.rm=TRUE)),
   .(countrycode, country)]                    -> countryinfo

dt_del_cols(countryinfo, "countrycode")

countryinfo[, .(itemcount=sum(itemcount),
                bibcount=sum(bibcount),
                date.div=mean(date.div),
                fy17_circ=sum(fy17_circ),
                fy18_circ=sum(fy18_circ),
                fy19_circ=sum(fy19_circ),
                fy20_circ=sum(fy20_circ),
                fy21_circ=sum(fy21_circ),
                total_circ=sum(total_circ)),
  country]                                    -> countryinfo

countryinfo[order(-itemcount)]

countryinfo[, controlled_circ:=total_circ/itemcount]
countryinfo[, percent_coll:=itemcount/sum(itemcount)]

cp_lb_attributes(dat, countryinfo)
countryinfo %>% fwrite_plus_date("./target/countryinfo.dat")



# normalization

# countrynorm <- copy(countryinfo)
#
# countrynorm[, itemcount:=normalize(itemcount, method="range")]
# countrynorm[, date.div:=normalize(date.div, method="range")]
# countrynorm[, total_circ:=normalize(total_circ, method="range")]
# countrynorm[controlled_circ>2, controlled_circ:=1]    # HACK
# countrynorm[, controlled_circ:=normalize(controlled_circ, method="range")]
# countrynorm
#
# dt_keep_cols(countrynorm,  c("country", "itemcount", "date.div",
#                              "total_circ", "controlled_circ"))
# countrynorm %>% fwrite("./countrynorm.dat")



# --------------------------------------------------------------- #
# -----------            CENTER/LOCATION         -----------------#
# -----------            CENTER/LOCATION         -----------------#
# --------------------------------------------------------------- #

locxwalk <- readRDS("../../crosswalks/locationxwalk.datatable")

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

cp_lb_attributes(dat, centerinfo)
centerinfo %>% fwrite_plus_date("./target/centerinfo.dat")




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

add.to.build.a.count("gen_info", "fy21circ",
                     sprintf("%d", dat[, sum(fy21_checkouts, na.rm=TRUE)]),
                     "Total check-outs in FY21")

build.a.count <- build.a.count[dacat!=""]

cp_lb_attributes(dat, build.a.count)
build.a.count %>% fwrite_plus_date("./target/gen-info.dat")



