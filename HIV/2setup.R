setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# required packages for HIV Testing Code.

req.packages <- c(
  "data.table", # data.table
  "RODBC", # odbcConnect, sqlQuery, odbcCloseAll
  "tibble", # as_tibble
  "dplyr", # filter, slice, %>%, subset, select
  "stringr", # str_to_upper
  "lubridate" # dmy
  )

lapply(req.packages,
       require,
       character.only = TRUE,
       quietly = TRUE
)
rm(req.packages)
(.packages())
list(ls())
