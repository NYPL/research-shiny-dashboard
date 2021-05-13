
# NYPL Research Dashboard

The code in this repository can be roughly divided into two parts

1. The [Shiny](https://shiny.rstudio.com/) UI and Server code (R) that
power the
[NYPL Research Library metrics dashboard](https://nypldigital.shinyapps.io/research-dashboard/).
This is, primarily, in `./the-app/app.R`

2. The R/sh scripts that generate the data that feeds into the dashboard.
These are the scripts in `./making-the-data/make-it-all/`.

We'll discuss these two parts below

### dashboard

A PDF slideshow explaining this dashboard's raison d'etre can be
compiled from the markdown file
`./documentation/research-dashboard-presentation-slideshow.md` using Pandoc.
Use the Makefile in the containing directory.

The production dashboard can be viewed at the following link...

[https://nypldigital.shinyapps.io/research-dashboard/](https://nypldigital.shinyapps.io/research-dashboard/)

The staging/testing/development version of dashboard can be viewed at
here...

[https://tonyfischetti.shinyapps.io/nypl-research/](https://tonyfischetti.shinyapps.io/nypl-research/)


### making the data foundation

Since the dashboard loads the data from scratch everytime it's deployed,
the data files that the dashboard feeds off of need to be spartan.
Additionally, no heavy data processing should happen in the dashboard
layer since it should be as responsive as possible.

Because of this, the scripts in `./making-the-data/make-it-all/` take
data from various places and process them into nice data files that
require little or no further processing once consumed by the dashboard.

Perhaps fun to note is that the data sources used by these scripts are,
themselves, derivated data sets from still other upstream resources.

At time of writing, the data processed by the scripts in
`./making-the-data/make-it-all/` for use in the dashboard
come from four sources:

1. A data product from the exported SQL from the Shadow Sierra database.
   [Repo link](https://github.com/NYPL/sierra-shadow-dataset)

2. A data product from the raw EZProxy web logs.
   [Repo link](https://github.com/NYPL/ezproxy-stats/)

3. A data product from a maintained spreadsheet of Scan and Deliver
   requests. [Repo link](https://github.com/NYPL/scan-and-deliver-stats)

4. A data product derived from ReCAP's SCSB MarcXML exports and exported
   GFA data. [Repo link](https://github.com/recap-assessment-team/compile-recap-stats)

Currently, the dashboard is also using static data files for visits
and circulation data from "Web Management Reports" and Lair's
"Materials and Readers Consulted" but those will soon be deprecated in
favor of new sources that allow for a more principled and reproducible
data pipeline.

