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


RECAP_DATA_LOCATION     <- "~/Dropbox/NYPL/compile-recap-stats/target/"



dat <- fread_plus_date(sprintf("%s/RECAP.dat.gz", RECAP_DATA_LOCATION))
set_lb_attribute(dat, "source", "SCSB MARCXml export")
set_lb_attribute(dat, "note", "derived from data substrate from 'https://github.com/recap-assessment-team/compile-recap-stats")
attributes(dat)

non_nypl_titles <- dat[inst_has_item!="NYPL", uniqueN(scsbid)]
non_nypl_items  <- dat[inst_has_item!="NYPL", uniqueN(barcode)]

recap_gen_info <- data.table(variable=c("non-nypl-items", "non-nypl-titles"),
                             value=c(non_nypl_items, non_nypl_titles))

cp_lb_attributes(dat, recap_gen_info)
recap_gen_info %>% fwrite_plus_date("./target/recap-gen-info.dat")



