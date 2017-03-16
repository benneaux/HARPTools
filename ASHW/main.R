# Generates ASHW Report and saves it as a word .docx
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(knitr) # spin
library(rmarkdown) # render
enddate <- "'01/01/2017'" # must be in mm/dd/yyyy
spin("testreport.R", 
     knit = FALSE, 
     report = TRUE, 
     format = "Rmd"
     )

render("testreport.Rmd", clean = TRUE, quiet = TRUE)

rm(list = ls())