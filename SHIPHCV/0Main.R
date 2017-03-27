# This will produce the HCV reports from SHIP for checking.
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RODBC)

VHSExclude <<- TRUE # Should the report include clients also in the VHS database?

startdate <- as.Date("2017/01/01")
enddate <- as.Date("2017/03/26")

source("1SQL.R")

source("2DataManipulation.R")

source("3Export.R")