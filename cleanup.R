require(dplyr)
require(tidyr)
require(data.table)
require(readxl)
require(reshape2)
require(stringr)

## Import ====================================

file.name <- "Dispensing_HCV July 2016.XLS" # name of the file in quotes

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

    junk.s <- 7 # the number at the start
    junk.e <- 2 # the number at the end

      clean.data <- import.data %>%
        na.omit(.[,1]) %>%
        slice(junk.s:(n()-junk.e))

      temp <- clean.data %>%
        subset(select = -1) %>%
        filter(is.na(.[,1])== FALSE)

      temp.a <- temp[ c(TRUE,FALSE), ] %>% subset(select = -3)
      temp.b <- temp[ !c(TRUE,FALSE), ] %>% subset(select = -2)
      data.b <- as.data.frame(cbind(temp.a,temp.b))
      names(data.b) <- c("Units","Date","Cost","Code")
  
      rm(temp, temp.a, temp.b)

## Filter Sites =====================
  
  data <- clean.data 
  
  temp <- data %>%
    subset(select = 1) %>%
    filter(is.na(.[,1])== FALSE)
  
    
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
  
  clean.data <- cbind(
    temp,
    rep(
      temp.val$X1,
      temp.ind.diff
      )
    )
  
  clean.data <- clean.data[-c(temp.ind),]

  rm(data, temp, temp.val, temp.ind, temp.ind.diff)
  
## Filter Cost Centres ================================= 

  # Use the output from the previous section.

  
  data <- clean.data
    
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
  
  clean.data <- cbind(
    data,
    rep(
      temp.val$X1,
      temp.ind.diff
    )
  )
  clean.data <- clean.data[-c(temp.ind),]
  
  rm(data, temp, temp.val, temp.ind, temp.ind.diff)
## Split the first column into MRNs and Drugs ============

  data <- clean.data
  
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