# Combines HBV Dispensing Report Data.

## Setup =======================================================================
req.packages <- c("dplyr","tidyr","data.table","readxl","stringr","lubridate")

lapply(
  req.packages,
  require,
  character.only = TRUE,
  quietly = TRUE
)
rm(req.packages)

## Import ====================================

Program <- "HBV" # i.e. HBV or HCV
Type <- "Purchasing"
Period_Num <- "12" # 01 for January etc.
Period <- "December" # i.e. Month, FY
Year <- "2016"
# name of the file in quotes 
#NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!
file.path <- paste("O:/HARP_Data/01 Reports/",Program,"/2016/",Program," Treatment Dispensing/",Period_Num," ",Period,"/02 Analysis/",sep="")
file.name <- paste(Program,"Combined",Type,Period,Year,sep = "_")
full.path <- paste(file.path,file.name, sep = "")

data <- rbind(
  Lamivudine,
  Tenofovir,
  Adefovir,
  Entecavir
  )

setwd(file.path)
write.csv(
  data,
  paste0(file.name,".csv"),
  row.names = FALSE
)

