###############################################################################
#
#HBV Ordering - Run entire block
#
###############################################################################
  
  rm(list =ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Period_Num <- "02" # 01 for January etc.
  Year <- "2017"
  Program <- "HBV"
  
  source("2setup.R", echo = FALSE)
  source("3functions.r", echo = FALSE)
  source("4Ordering.R")

###############################################################################
#
#HBV Dispensing - Run entire block
#
###############################################################################
  
  rm(list =ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Period_Num <- "02" # 01 for January etc.
  Year <- "2017"
  Program <- "HBV"
  
  source("2setup.R", echo = FALSE)
  source("3functions.r", echo = FALSE)
  source("4Dispensing.R")
  
###############################################################################
#
#HCV Ordering - Run entire block
#
###############################################################################
  
  rm(list =ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Period_Num <- "02" # 01 for January etc.
  Year <- "2017"
  Program <- "HCV"
  
  source("2setup.R", echo = FALSE)
  source("3functions.r", echo = FALSE)
  source("4Ordering.R")

###############################################################################
#
#HCV Dispensing - Run entire block
#
###############################################################################
  
  rm(list =ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Period_Num <- "02" # 01 for January etc.
  Year <- "2017"
  Program <- "HCV"
  
  source("2setup.R", echo = FALSE)
  source("3functions.r", echo = FALSE)
  source("4Dispensing.R")
