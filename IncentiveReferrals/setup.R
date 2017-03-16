## Setup =======================================================================
req.packages <- c("XLConnect","reshape2","dplyr","tidyr","data.table","readxl","stringr","lubridate")

lapply(
  req.packages,
  require,
  character.only = TRUE,
  quietly = TRUE
)
rm(req.packages)