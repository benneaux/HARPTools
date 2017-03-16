setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# required packages for HBV/HCV Dispensing and Ordering Code.

req.packages <- c(
  "tibble", # as_tibble
  "dplyr", # filter, slice, %>%, subset, select
  "stringr", # str_replace_all, str_trim, str_detect, str_split, str_split_fixed
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
