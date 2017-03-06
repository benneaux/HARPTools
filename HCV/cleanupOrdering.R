# Cleans up HCV Ordering Report.
rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions.r")
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
Period_Num <- "02" # 01 for January etc.
Year <- "2017"
Period <- num2month(Period_Num)
PeriodAbbr <- num2month(Period_Num, TRUE)# i.e. Month, FY
file.path <- paste("O:/HARP_Data/01 Reports/HCV/",Year,"/","HCV Treatment Dispensing/",Period_Num," ",Period,"/01 Data/",sep="") 
files <- tibble::as_tibble(base::list.files(file.path, pattern = ".XLS"))
file.names <- dplyr::filter(files, !grepl('HEP|HEPC|HCV', value))
combined <- NULL

for(i in 1:nrow(file.names)){
  
  
  Drug <- file.names$value[i]
  Drug_rem_spec_chars <- Drug %>% 
    str_replace_all("[p]urchasing","") %>%
    str_replace_all("&","and") %>%
    str_replace_all(Period,"") %>%
    str_replace_all(PeriodAbbr,"") %>%
    str_replace_all(Year,"") %>%
    str_replace_all(".XLS","") %>%
    str_replace_all("_","") %>%
    str_trim(side = "both")

  file.name <- paste(Drug)
  full.path <- paste(file.path,file.name, sep = "")
  report.path <- paste("O:/HARP_Data/01 Reports/HCV/",Year,"/","HCV Treatment Dispensing/",Period_Num," ",Period,"/02 Analysis/",sep="")
  import.data <- importdata(full.path) 

## Setup ================================

# First, check the data to see how many
# junk rows there are a the start and 
# end of the dataset.

# Create clean.data by removing blank rows.
clean.data <- import.data[
  rowSums(is.na(import.data))# Calculates no. of NAs in each row 
  != ncol(import.data),] # Calculates number of columns present.


junk.s <- 5 # the number at the start
junk.e <- 1 # the number at the end     

clean.data <- clean.data %>%
  slice(junk.s:(n()-junk.e))

# Separate out data fields from all cols except 1.    
temp <- clean.data %>%
  subset(select = -1)

temp <- temp[
  rowSums(is.na(temp)) 
  != ncol(temp),]

temp <- temp[!is.na(temp$X6),]
temp <- temp[,colSums(is.na(temp)) != nrow(temp)]
temp <- subset(temp, select=-3)
names(temp) <- c("Cost","Units","Tax","Cost+Tax")

data.b <- temp
rm(junk.s,junk.e,temp)

## Filter Hospitals =====================

# Select the first column from our data
temp <- clean.data %>%
  subset(select = 1)

# Find the index of each row containing the string 'Site: ".  
temp.ind <- which(
  str_detect(
    temp$X1,
    "Hospital"
  )
  &!
  str_detect(
    temp$X1,
    "Hospital HCD"
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

if(length(temp.ind.diff)==0){
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)
}
# This merges the original data with another column of "Sites: ....", replicated
# by the numbers in the diff vector.
temp2 <- cbind(
  temp,
  rep(
    temp.val$X1,
    temp.ind.diff
  )
)

# This removes the "Site: ..." rows from the original column vector, thus 
# finally separating the site data into a separate (column) variable from the 
# rest of the data.
temp2 <- temp2[-c(temp.ind),]

# The following line of code can help resolve indivual issues
# you may encounter with the data: in this example, a client's
# name has been appended to their MRN. The code replaces the 
# Combined field with their MRN only.


data.a <- temp2
rm(temp2, temp, temp.val, temp.ind, temp.ind.diff)

condition <- c(
  str_detect(
    data.a[,1],
    "Pty"
    )
  |
  str_detect(
    data.a[,1],
    "HCD"
  )
  |
  str_detect(
    data.a[,1],
    "Symbion"
  )
)

if(TRUE %in% condition){
  ## Filter Suppliers =====================

# Select the first column from our data
temp <- data.a %>%
  subset(select = 1)
temp2 <- data.a %>%
  subset(select = -1)

# Find the index of each row containing the string 'Site: ".  
temp.ind <- which(condition)

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
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)- 1] + 2 
}

if(length(temp.ind.diff)==0){
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)
}

# This merges the original data with another column of "Sites: ....", replicated
# by the numbers in the diff vector.
temp <- cbind(
  temp,
  rep(
    temp.val[,1],
    temp.ind.diff
  )
)

# This removes the "Site: ..." rows from the original column vector, thus 
# finally separating the site data into a separate (column) variable from the 
# rest of the data.
temp3 <- cbind(temp,temp2)
temp3 <- temp3[-c(temp.ind),]
data.a <- temp3
}
# The following line of code can help resolve indivual issues
# you may encounter with the data: in this example, a client's
# name has been appended to their MRN. The code replaces the 
# Combined field with their MRN only.

rm(temp3, temp2, temp, temp.val, temp.ind, temp.ind.diff)
condition <- c(str_detect(data.a[,1],"TAB")|str_detect(data.a[,1],"units"))
if(TRUE %in% condition){
  ## Filter Drugs =====================

# Select the first column from our data
temp <- data.a %>%
  subset(select = 1)
temp2 <- data.a %>%
  subset(select = -1)

# Find the index of each row containing the string 'TAB: ".  
temp.ind <- which(condition)

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
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)- 1] + 2 
}

if(length(temp.ind.diff)==0){
  temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)
}
# This merges the original data with another column of "Sites: ....", replicated
# by the numbers in the diff vector.
temp <- cbind(
  temp,
  rep(
    temp.val$X1,
    temp.ind.diff
  )
)

# This removes the "Site: ..." rows from the original column vector, thus 
# finally separating the site data into a separate (column) variable from the 
# rest of the data.
temp3 <- cbind(temp,temp2)
temp3 <- temp3[-c(temp.ind),]

# The following line of code can help resolve indivual issues
# you may encounter with the data: in this example, a client's
# name has been appended to their MRN. The code replaces the 
# Combined field with their MRN only.


data.a <- temp3
rm(temp3, temp2, temp, temp.val, temp.ind, temp.ind.diff)

}
## Split Date and Order =======================================================

# Select the first column from our data
temp <- data.a %>%
  subset(select = 1)
temp2 <- data.a %>%
  subset(select = -1)

# Find the index of each row containing the string 'Site: ".  
orders <- str_split(temp[,1], "  - ") 
orders <- data.table(unlist(orders))
dates <- unlist(c(orders[c(TRUE,FALSE),]))
ordernos <- unlist(c(orders[c(FALSE,TRUE),]))
ordernos <- str_replace(ordernos, "Order no. ","")

data.a[,1] <- dates

data.a <- cbind(data.a,ordernos)
names(data.a) <- c("Date","Drug","Supplier","Hospital", "OrderNo")

## Final ======================================================================

final.data <- cbind(data.a,data.b)
final.data <- final.data[,c(4,2,1,6,7,8,9,3,5)]

write.csv(
  final.data,
  paste0(report.path, 
         paste(Program,
               Period,
               Year,
               Type,
               Drug_rem_spec_chars,
               sep = "_"),
         ".csv"),
  row.names = FALSE
)

assign(Drug_rem_spec_chars,
       final.data)
combined <- rbind(combined,
                  final.data)
}

file.name <- paste0(report.path, 
                    paste(Program,
                          Period,
                          Year,
                          Type,
                          "Combined",
                          sep = "_"),
                    ".csv")

write.csv(
  combined,
  file.name,
  row.names = FALSE
)
rm(list = ls())
