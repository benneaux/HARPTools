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

Program <- "HCV" # i.e. HBV or HCV
Type <- "Purchasing"
Period_Num <- "09" # 01 for January etc.
Period <- "September" # i.e. Month, FY

# name of the file in quotes 
#NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!
file.path <- paste("O:/HARP_Data/01 Reports/",Program,"/2016/",Program," Treatment Dispensing/",Period_Num," ",Period,"/02 Analysis/",sep="")
file.name <- paste(Type,"Combined_Sept_2016.csv", sep = "_")
full.path <- paste(file.path,file.name, sep = "")

data <- rbind(
  Ribavirin,
  Daclatasvir,
  Sofosbuvir_and_Sofosbuvir_Ledipasvir,
  Interferon
  )

setwd(file.path)
write.csv(
  data,
  paste(Program,Period,file.name,sep="_"),
  row.names = FALSE
)

