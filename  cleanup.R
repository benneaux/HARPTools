require(dplyr)
require(tidyr)
require(data.table)
require(readxl)
require(reshape2)
require(stringr)

## Import ====================================

file.name <- "HBV July 2015_June 2016.XLS" # name of the file in quotes

import.data <- 
  as.data.table(
    read_excel(
      file.name,
      col_names = FALSE,
      sheet = 1
      )
    )

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
    temp<- temp[!str_detect(temp[,1],"Dispensed") | is.na(temp[,1]), ]
    # Removes empty columns
    temp<- temp[,colSums(is.na(temp)) != nrow(temp)]
    
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
    names(data.b) <- c("Prescriber","Units","Date","Cost","Code")
    
    # Removes unnescessary text from "Prescriber:"
    data.b[,"Prescriber"] <- str_replace(
      data.b[,"Prescriber"],
      "Prescriber: ",
      ""
      )
    # Removes unnescessary text from "Date:"
    data.b[,"Date"] <- str_replace(
      data.b[,"Date"],
      "Date: ",
      ""
    )

  # Clear temporary variables.
  rm(temp, temp.a, temp.b)

## Filter Sites =====================
  
  data <- clean.data 
  
  temp <- data %>%
    subset(select = 1)
  
    
  temp.ind <- which(
    str_detect(
      temp$X1,
      "Site: "
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
    
    if(length(temp)<temp.ind[length(temp.ind)]){
      temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)] + 1
    } else {
      temp.ind.diff[length(temp.ind.diff)+1] <- nrow(temp)-temp.ind[length(temp.ind)- 1] + 1  
    }
  
  clean.data2 <- cbind(
    temp,
    rep(
      temp.val$X1,
      temp.ind.diff
      )
    )
  
  clean.data2 <- clean.data2[-c(temp.ind),]

  rm(data, temp, temp.val, temp.ind, temp.ind.diff)
  
## Filter Cost Centres ================================= 

  # Use the output from the previous section.
  data <- clean.data2
    
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
    temp.ind.diff[(length(temp.ind.diff)+1)] <- nrow(temp)- temp.ind[(length(temp.ind)-1)] + 1  
  }
  
  clean.data3 <- cbind(
    data,
    rep(
      temp.val$X1,
      temp.ind.diff
    )
  )
  clean.data3 <- clean.data3[-c(temp.ind),]
  
  rm(data, temp, temp.val, temp.ind, temp.ind.diff)

## Up to here!!!!!!!! ============

# NOTE: NEED TO CLEAR OUT INPATIENT, OUTPATIENT & DISCHARGE FROM COL.
break() 
  data <- clean.data3
  
  temp <- data %>%
    subset(select = 1)
  
  temp.mrn <- temp[ c(TRUE,FALSE),]
  temp.drg <- temp[ !c(TRUE,FALSE),]
  temp.comb <- cbind(temp.mrn,temp.drg)
  
  data.a <- cbind(temp.comb, clean.data[,2:ncol(clean.data)])
  names(data.a) <- c("MRN","Drug_Disp","Site","Cost_Centre")

  rm(clean.data, data, import.data, temp, temp.comb, temp.mrn, temp.drg)
## Concatenate both datasets
  
  final <- cbind(data.a, data.b)