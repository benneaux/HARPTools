rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

startdate <- "'10/01/2016'" # Note: Dates must be in wacky US format
enddate <- "'12/31/2016'"  # i.e mm/dd/yyyy

template.path <- "O:/HARP_Data/04 Templates/HIVSTI/" 
template.name <- "HIV testing and CD4 reports layout.xlsx"
full.path <- paste0(template.path,template.name)
scratch.path <- "O:/HARP_Data/01 Reports/Scratch/"
template <- XLConnect::loadWorkbook(full.path, 
                                    create = FALSE)

#XLConnect::setStyleAction(template, XLC$"STYLE_ACTION.PREDEFINED")

source("2setup.R")
source("4SQL.R")
source("5Datamanipulation.R")
source("6write.R")
source("7save.R")

rm(list =ls())
