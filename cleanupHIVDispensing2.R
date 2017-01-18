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
Qtr_Num <- "04" # 01 for January - March etc.
Period <- "Oct-Dec" # i.e. Month, FY - Make it the same as the file the data is stored in.
Year <-  "2016"

# Name of the file in quotes 
# NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!
file.path <- paste("O:/HARP_Data/01 Reports/HIVSTI/2016/",Program," Treatment Dispensing/",Qtr_Num," ",Period,"/01 Data/",sep="") 

file.name <- paste("HIV dispensing October_December.xls")
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

names(clean.data) <- c("MRN","Prescriber","Cost","Disp_Date","PBS_Code")
## Clean Up MRNs ==============================================================  

# Separate out data fields from all cols except 1.    
temp <- clean.data %>%
  subset(select = -1)

# Removes "Number of Items Dispensed".
temp<- temp[!str_detect(temp[,1],"Di[spens]+ed") | is.na(temp[,1]), ]

# Removes "Prescriber: "
temp$Prescriber <- gsub("Prescriber: ", "", temp$Prescriber)

# Removes "Date: "
temp$Disp_Date <- gsub("Date: ", "", temp$Disp_Date)

temp <- temp[
  rowSums(is.na(temp)) 
  != ncol(temp),]

# Returns odd-numbered rows
temp.a <- temp[ c(TRUE, FALSE), ]
# Removes empty columns
temp.a <- temp.a[,colSums(is.na(temp.a)) != nrow(temp.a)]

names(temp.a) <- c("Prescriber","Units","Disp_Date")

# Returns even-numbered rows
temp.b <- temp[ !c(TRUE,FALSE), ]
# Removes empty columns
temp.b <- temp.b[,colSums(is.na(temp.b)) != nrow(temp.b)]
# Combines each odd-even pair into a single record.
data.b <- as.data.frame(cbind(temp.a,temp.b))

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
    temp$MRN,
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
    temp.val$MRN,
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
    temp$MRN,
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
    temp.val$MRN,
    temp.ind.diff
  )
)
clean.data3 <- clean.data3[-c(temp.ind),]

# The data contains information on the dispensing setting that is not
# necessary for our purposes. The next few lines removes these rows from
# the data.

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
final$Disp_Date <- lubridate::dmy(final$Disp_Date)

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

final_Reversals <- final[str_detect(final$Units,"-"),]
final <- final[!str_detect(final$Units,"-"),]

# Remove unnescessary variables.
rm(i, condition)

## Export Data =================================================================

# This code exports the data to .csv files in the working directory.

setwd(file.path)
write.csv(
  final,
  paste0(Program,"_Complete_Dispensing_",Period,"_",Year,".csv"),
  row.names = FALSE
)
write.csv(
  final_Reversals,
  paste0(Program,"_Reversals_",Period,"_",Year,".csv"),
  row.names = FALSE
)

Unique.clients <- uniqueN(final$MRN)
Unique.scripts <- nrow(final)
Total.cost <- sum(final$Cost)

Unique.clients
Unique.scripts
Total.cost
