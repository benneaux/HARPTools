# Cleans up HBV Dispensing Report.
rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions.r")
## Setup =======================================================================

req.packages <- c("tibble","dplyr","tidyr","data.table","readxl","stringr","lubridate","stringi")

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


Program <- "HBV" # i.e. HBV or HCV
Period_Num <- "02" # 01 for January etc.
Period <- num2month(Period_Num)
Year <- "2017"
# Name of the file in quotes 
# NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!

file.path <- paste("O:/HARP_Data/01 Reports/HBV/",Year,"/","HBV Treatment Dispensing/",Period_Num," ",Period,"/01 Data/",sep="") 
files <- tibble::as_tibble(list.files(file.path, pattern = ".XLS"))
file.name <- dplyr::filter(files, grepl('HEP|HEPB|HBV', value))

# Where the files will be saved
report.path <- paste("O:/HARP_Data/01 Reports/",Program,"/",Year,"/",Program," Treatment Dispensing/",Period_Num," ",Period,"/02 Analysis/",sep="")

full.path <- paste(file.path,file.name, sep = "")

import.data <- importdata(full.path)

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

# Removes "Dispensing Style: " rows from dataset.
clean.data <- clean.data[!str_detect(clean.data[,1],"Dispensing Style: "),]

# Separate out data fields from all cols except 1.    
temp <- clean.data %>%
  subset(select = -1)

temp <- temp[
  rowSums(is.na(temp)) 
  != ncol(temp),]

# Removes both "Dispensed by: " and "Number of Items Dispensed".
temp<- temp[!str_detect(temp[,1],"Dispens[ed]") | is.na(temp[,1]), ]
# Removes empty columns
temp<- temp[,colSums(is.na(temp)) != nrow(temp)]

# Moves Reversal column to end if present
if(TRUE %in% str_detect(temp[,],"Reversal")){
  rev <- which(str_detect(temp[,],"Reversal") == TRUE)
  temp <- temp %>% 
    select(-rev,everything())
  temp[,length(temp)] <- gsub("Reversal","Yes",temp[,length(temp)])
}

# Returns odd-numbered rows
temp.a <- temp[ c(TRUE, FALSE), ]
# Removes empty columns
temp.a <- temp.a[,colSums(is.na(temp.a)) != nrow(temp.a)]
# Returns even-numbered rows
temp.b <- temp[ !c(TRUE,FALSE), ]
# Removes empty columns
temp.b <- temp.b[,colSums(is.na(temp.b)) != nrow(temp.b)]
# Combines each odd-even pair into a single record.
data.b <- as.data.frame(cbind(temp.a,temp.b))
# Names columns
if(length(data.b)==6){
  names(data.b) <- c("Prescriber","Units","Date","Cost","Code","Reversal")
  for(i in 1:nrow(data.b)){
    if(is.na(data.b[i,length(data.b)])==TRUE){
      data.b[i,length(data.b)] = ""
    }
  }
} else {
  names(data.b) <- c("Prescriber","Units","Date","Cost","Code")
}
# Removes unnescessary text from "Prescriber:"
data.b[,"Prescriber"] <- gsub("Prescriber: ","",data.b[,"Prescriber"])
# Removes unnescessary text from "Date:"
data.b[,"Date"] <- gsub("Date: ","",data.b[,"Date"])

data <- clean.data
# Clear temporary variables.
rm(clean.data, temp, temp.a, temp.b, junk.e, junk.s)

## Filter Sites =====================

# Select the first column from our data
temp <- data %>%
  subset(select = 1)

# Find the index of each row containing the string 'Site: ".  
temp.ind <- which(
  str_detect(
    temp$X1,
    "Site: "
  )
)

# Find the difference between each index and save into a 
# vector of differences.
temp.ind.diff <- diff(
  temp.ind,
  lag=1
)

# Return a vector of only the values corres. to the indices
# found by temp.ind
temp.val <- slice(
  temp,
  temp.ind
)

# The heart of this statement is used to adjust for the fact that
# the vector of differences is too short (-1). This happens because
# the diff() function doesn't return the difference between the last
# value and the end of the data set - only the differences between values.

# The if accounts for an error that occurs when the last index = the number of
# rows in the data.

if(length(temp)<temp.ind[length(temp.ind)]){
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)] + 1
} else {
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)- 1] + 1  
}

# This merges the original data with another column of "Sites: ....", replicated
# by the numbers in the diff vector.
clean.data2 <- cbind(
  temp,
  rep(
    temp.val$X1,
    temp.ind.diff
  )
)

# This removes the "Site: ..." rows from the original column vector, thus 
# finally separating the site data into a separate (column) variable from the 
# rest of the data.
clean.data2 <- clean.data2[-c(temp.ind),]

# The following line of code can help resolve indivual issues
# you may encounter with the data: in this example, a client's
# name has been appended to their MRN. The code replaces the 
# Combined field with their MRN only.

#clean.data2[,1] <- gsub("999999: Mr Benjamin Moran", "999999", clean.data2[,1])
data <- clean.data2
rm(clean.data2, temp, temp.val, temp.ind, temp.ind.diff)

## Filter Cost Centres ================================= 

# The principles here are the same as the previous section, just accounting
# for a different variable (Cost Centres).

# Use the output from the previous section.


temp <- data %>%
  subset(select = 1)


temp.ind <- which(
  str_detect(
    temp$X1,
    ": "
  )
)

temp.ind.diff <- diff(
  temp.ind,
  lag=1
)

temp.val <- slice(
  temp,
  temp.ind
)

if(nrow(temp)!=temp.ind[length(temp.ind)]){
  temp.ind.diff[(length(temp.ind.diff)+1)] <- nrow(temp)-temp.ind[length(temp.ind)] + 1
} else {
  temp.ind.diff[(length(temp.ind.diff)+1)] <- nrow(temp)- temp.ind[(length(temp.ind)-1)]  
}

clean.data3 <- cbind(
  data,
  rep(
    temp.val$X1,
    temp.ind.diff
  )
)
clean.data3 <- clean.data3[-c(temp.ind),]

# The data contains information on the dispensing setting that is not
# necessary for our purposes. The next few lines removes these rows from
# the data.

clean.data3 <- clean.data3[!str_detect(clean.data3[,1],"Inpatient"),]
clean.data3 <- clean.data3[!str_detect(clean.data3[,1],"Outpatient"),]
clean.data3 <- clean.data3[!str_detect(clean.data3[,1],"Discharge"),]

rm(data, temp, temp.val, temp.ind, temp.ind.diff)

## Split MRNs and Drugs ==================================

# This step will end with all variables separated into individual columns, 
# with each row representing a single observation/record.

data <- clean.data3

temp <- data %>%
  subset(select = 1)
temp2 <- data %>%
  subset(select = -1)

temp.mrn <- temp[ c(TRUE,FALSE),]
temp.drg <- temp[ !c(TRUE,FALSE),]
temp.other <- temp2[ c(TRUE,FALSE),]
data.a <- cbind(temp.mrn,temp.drg,temp.other)

names(data.a) <- c("MRN","Drug_Disp","Site","Cost_Centre")

rm(clean.data3, data, import.data, temp, temp2, temp.mrn, temp.drg, temp.other)

## Concatenate both datasets

final <- cbind(data.a, data.b)

rm(data.a, data.b)


## Clean Up MRNs ==============================================================  
# we need to do some minor housekeeping on the data.

# Converts (letter) O into (number) 0. Sometimes MRNs come with  leading 
# zeroes replaced to prevent Excel trimming them. We don't want them.
final$MRN<- gsub("O", 0, final$MRN)

# Removes # marks placed at the front of some of the MRNs.
final$MRN <- gsub("#", "", final$MRN)

# Removes unnescessary text from the Site variable.
final$Site <- gsub("Site: ","", final$Site)

# Rounds the Cost variable to 2 decimal places so that it reads accurately as
# dollars and cents.
final$Cost <- round(final$Cost,2)

# Reformats Date variable from character to date.
final$Date <- lubridate::dmy(final$Date)

# Some of the MRNs are recognised as numbers and R formats them accordingly,
# i.e. with decimal places. After some other conversions they become 
# characters again, so we need to trim the decimals.

condition <- str_detect(final$MRN,"[.+]")==TRUE
for(i in 1:nrow(final)){
  if(condition[i]){
    final[i,1] = gsub("\\..*","",final[i,1])
  }
}

# Separate Codes and Names into separate columns
final$Site_Code <- str_split_fixed(
  str_split_fixed(
    final$Site,
    " \\(",2)[,2],
  "\\)"
  ,2
)[,1]
final$Site <- str_split_fixed(final$Site," \\(",2)[,1]

final$Cost_Centre_Code <- str_split_fixed(final$Cost_Centre,"\\: ",2)[,1]
final$Cost_Centre <- str_split_fixed(final$Cost_Centre,"\\: ",2)[,2]

if("Reversal" %in% names(final)){
  final <- select(final, 1:2,11,3,12,everything())
} else {
  final <- select(final, 1:2,10,3,11,everything())
}
# Remove unnescessary variables.
rm(i)

## Export Data =================================================================

# This code exports the data to .csv files in the working directory.


# All Dispensing
final.Dispensing <- final %>%
  filter(., Cost>=0)

# All Reversals
final.Reversals <- final %>%
  filter(.,Cost<0)

setwd(report.path)
write.csv(
  final,
  paste0(Program,"_Complete_Dispensing_",Period,"_",Year,".csv"),
  row.names = FALSE
)
write.csv(
  final.Dispensing,
  paste(Program,Period,"Dispensing.csv",sep = "_"),
  row.names = FALSE
)
write.csv(
  final.Reversals,
  paste(Program, Period,"Reversals.csv",sep = "_"),
  row.names = FALSE
)

Unique.clients <- uniqueN(final.Dispensing$MRN)
Unique.scripts <- nrow(final.Dispensing)
Total.cost <- sum(final.Dispensing$Cost)

Unique.clients
Unique.scripts
Total.cost
