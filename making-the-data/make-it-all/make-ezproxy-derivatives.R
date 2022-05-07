#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(width = 80)
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
library(libbib)
library(BBmisc)
# ------------------------------ #


EZPROXY_DATA_LOCATION   <- "~/Dropbox/NYPL/ezproxy-logs/target/"



allez <- sapply(sort(list.files(EZPROXY_DATA_LOCATION)),
                function(x) sprintf("%s/%s", EZPROXY_DATA_LOCATION, x))

colsineed <- c("session", "ptype", #"date_and_time",
               "vendor", "url", "barcode",
               "homebranch", "patroncreatedate", "extract", "just_date")

system.time(
allez <- rbindlist(lapply(allez,
                function(x){ fread_plus_date(x, select=colsineed) }))
)

setorder(allez, just_date)
set_lb_date(allez, allez[.N, just_date])


allez <- allez[!(vendor %chin% c("ezproxy", "google")) & !is.na(vendor)]
short <- allez[, .(unique_sessions=uniqueN(session)), .(just_date, vendor)]

short[, sum(unique_sessions), vendor][
  order(-V1)][, .(vendor, venrank=frank(-V1, ties.method="first"))] -> venrank
setkey(venrank, "vendor")
setkey(short, "vendor")
uniq_sessions_by_dates_and_vendor <- venrank[venrank<31][short, nomatch=NULL]



# this is unprincipled, but I have no choice (anomaly)
uniq_sessions_by_dates_and_vendor[vendor=="proquest" & unique_sessions>1000, unique_sessions:=700]


# ok, we have to fix this for all vendors (Fri 06 May 2022 02:26:30 PM EDT)
uniq_sessions_by_dates_and_vendor[, fake:=shift(unique_sessions, 7), vendor]
uniq_sessions_by_dates_and_vendor[just_date >= as.Date("2021-07-31") &
                                    just_date <= as.Date("2021-08-04"),
                                  unique_sessions:=fake]
uniq_sessions_by_dates_and_vendor[, fake:=NULL]


# totals
totals <- allez[, .(vendor="TOTAL", venrank=0, unique_sessions=uniqueN(session)), just_date]
uniq_sessions_by_dates_and_vendor <- rbind(uniq_sessions_by_dates_and_vendor, totals)



cp_lb_attributes(allez, uniq_sessions_by_dates_and_vendor)
uniq_sessions_by_dates_and_vendor %>%
  fwrite_plus_date("./target/ezproxy-vendor-dates.dat")



### ptype distribution
allez[!duplicated(barcode), .(count=.N), ptype][!is.na(ptype)] -> ptypedist
setorder(ptypedist, -count)
ptypedist[, percent:=round(100*count/sum(count), 2)]

cp_lb_attributes(allez, ptypedist)
ptypedist %>% fwrite_plus_date("./target/ezproxy-ptype-dist.dat")


