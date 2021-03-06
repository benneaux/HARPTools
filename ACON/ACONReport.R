#'---
#'title: "HNELHD ACON Report"
#'date: "`r Sys.Date()`"
#'output:  pdf_document
#'---

#+ packages, include = FALSE
library(knitr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(pander)
library(rmarkdown)
library(grid)
opts_chunk$set(dev = 'postscript', fig.width = 10, fig.height = 6)
opts_chunk$set(pander::set.alignment(default = "center"))
opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)
enddate2 <- as.POSIXct(mdy(gsub("'","",enddate)))
enddate2 <- format(as.Date(enddate2), "%d/%m/%Y")
#'

#' This report details Occasions of Service for Aboriginal and/or Torres Strait Islander Clients in Tamworth, by month up to `r enddate2`.
#' 

#+ functions, include = FALSE
source("99functions.R")
#'

#+ SQL, include=FALSE
source("1SQL.R")
#'
#+ Data, include=FALSE
source("2Datamanipulation.R")
source("3Filtering.R")
#'
#'<center>
#+ clientsmonth, echo=FALSE, fig.align = "center"
pander(MonthlyData)
#'
#'</center>
#'<br>
#' In the below chart, the highest (green) and lowest (red) values are marked on the y-axis.
#+ chartclientsmonth, echo = FALSE, dvi = 10000, fig.align = "center"
year.period.plot(clientsmonth,"Month","n", "ACON Clients by Month", "# Unique Clients")
#'
#+ chartTestsmonth, echo = FALSE, dvi = 10000, fig.align = "center"
year.period.plot(HIVTestsmonth,"Month","n", "ACON HIVTests by Month", "# HIV Tests")
#'
#+ chartMSMmonth, echo = FALSE, dvi = 10000, fig.align = "center"
year.period.plot(HIVTestsMSMmonth,"Month","n", "ACON HIV Tests by Month (MSM)", "# HIV Tests (MSM)")
#'
#'<center>
#+ clientsquarter, echo=FALSE, fig.align = "center"
pander(QuarterlyData)
#'
#'</center>
#'<br>
#' In the below chart, the highest (green) and lowest (red) values are marked on the y-axis.
#+ chartclientsquarter, echo = FALSE, dvi = 10000, fig.align = "center"
year.period.plot(clientsquarter,"Quarter","n", "ACON Clients by Quarter", "# Unique Clients")
#'
