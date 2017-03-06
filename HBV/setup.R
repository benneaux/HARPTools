rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

req.packages <- c("tibble","dplyr","tidyr","data.table","readxl","stringr","lubridate","stringi")

lapply(req.packages,
       require,
       character.only = TRUE,
       quietly = TRUE
)
rm(req.packages)
(.packages())
list(ls())

Period_Num <- "02" # 01 for January etc.
Year <- "2017"