# Generates ASHW Report and saves it as a word .docx
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(knitr) # spin
library(rmarkdown) # render
library(dplyr) # %>%
library(stringi)
enddate <- "'12/31/2016'" # must be in mm/dd/yyyy

spin("ASHWReport.R", 
     knit = FALSE, 
     report = TRUE, 
     format = "Rmd"
     )

render("ASHWReport.Rmd", clean = TRUE, quiet = TRUE)

write.csv(tbldata, "ASHWReportData.csv")
rm(list = ls())