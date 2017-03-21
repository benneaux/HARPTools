# Generates ACON Report and saves it as a word .pdf
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(knitr) # spin
library(rmarkdown) # render
library(dplyr) # %>%
library(stringi)
enddate <- "'12/31/2016'" # must be in mm/dd/yyyy

spin("ACONReport.R", 
     knit = FALSE, 
     report = TRUE, 
     format = "Rmd"
     )

render("ACONReport.Rmd", clean = TRUE, quiet = TRUE)

write.csv(tbldata, "ACONReportData.csv")
rm(list = ls())