# Cleans up HIV Dispensing Report.

## Setup =======================================================================
rm(list =ls())
req.packages <- c("dplyr","tidyr","data.table","readxl","stringr","lubridate","stringi")

lapply(
  req.packages,
  require,
  character.only = TRUE,
  quietly = TRUE
)
rm(req.packages)
(.packages())
list(ls())
## Import ====================================

Program <- "HIV" 
Qtr_Num <- "03" # 01 for January - March etc.
Period <- "Jul-Sep" # i.e. Month, FY - Make it the same as the file the data is stored in.

# Name of the file in quotes 
# NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!
file.path <- paste("O:/HARP_Data/01 Reports/HIVSTI/2016/",Program," Treatment Dispensing/",Qtr_Num," ",Period,"/01 Data/",sep="") 

file.name <- paste("HIV July_Sept 2016.xls")
full.path <- paste(file.path,file.name, sep = "")
scratch.path <- "O:/HARP_Data/01 Reports/Scratch/"

import.data <- 
  as.data.table(
    read_excel(
      full.path,
      col_names = FALSE,
      sheet = 1
    )
  )
rm(file.name)

## Setup ================================

# First, check the data to see how many
# junk rows there are a the start and 
# end of the dataset.

# Create clean.data by removing blank rows.
clean.data <- import.data[
  rowSums(is.na(import.data))# Calculates no. of NAs in each row 
  != ncol(import.data),] # Calculates number of columns present.


junk.s <- 7 # the number at the start
junk.e <- 2 # the number at the end     

clean.data <- clean.data %>%
  slice(junk.s:(n()-junk.e))
clean.data[,4] <- ""

names(clean.data) <- c("MRN","Disp_Date","PBS_Code","Notes")
## Clean Up MRNs ==============================================================  
# we need to do some minor housekeeping on the data.

# Converts (letter) O into (number) 0. Sometimes MRNs come with  leading 
# zeroes replaced to prevent Excel trimming them. We don't want them.
clean.data$MRN<- gsub("O", 0, clean.data$MRN)

# Removes # marks placed at the front of some of the MRNs.
clean.data$MRN <- gsub("#", "", clean.data$MRN)

## Export Data =================================================================

# This code exports the data to .csv files in the working directory.

setwd(scratch.path)
write.csv(
  clean.data,
  "NewDispensing.csv",
  row.names = FALSE
)

Unique.clients <- uniqueN(clean.data$MRN)

Unique.clients
