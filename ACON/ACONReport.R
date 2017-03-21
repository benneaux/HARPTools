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
opts_chunk$set(dev = 'postscript', fig.width = 10, fig.height = 5)
opts_chunk$set(pander::set.alignment(default = "center"))
opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)
enddate2 <- as.POSIXct(mdy(gsub("'","",enddate)))
enddate2 <- format(as.Date(enddate2), "%d/%m/%Y")
#'

#' This report details Occasions of Service for Aboriginal and/or Torres Strait Islander Clients in Tamworth, by month up to `r enddate2`.
#' 

#+ SQL, include=FALSE
source("1SQL.R")
#'
#+ Data, include=FALSE
source("2Datamanipulation.R")
#'
#'<center>
#+ report, echo=FALSE, fig.align = "center"
pander(tbldata)
#'
#'</center>
#'<br>
#' In the below chart, the highest (green) and lowest (red) values are marked on the y-axis.
#+ chart, echo = FALSE, dvi = 10000, fig.align = "center"
source("3Charts.R")
#'
