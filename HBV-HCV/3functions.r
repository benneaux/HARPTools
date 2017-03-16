require(
  "lubridate", # month
  "tibble", #as_tibble
  "readxl" # read_excel
)

#' Return the Month name or Month name abbreviation
#' \code{num2month} returns the name (or abbr) of the month corres. to the 
#' integer input (1-12)
#' @param num A number from 1-12, representing a month.
#' @param abbreviation TRUE or FALSE, stating if you want the abbreviation.
#'
#' @return Either the full name of the month or the abbreviated name.
#'
#' @examples
#' num2month(2); will return 'February'
#' num2month(2,TRUE); will return 'Feb'. 
#' num2month(2,FALSE);  will return 'February' 
#' 
num2month <- function(num, abbreviation){
  as.character(
    month(
      as.numeric(num),
      label = TRUE,
      if(missing(abbreviation)){
        abbr = FALSE
      } else {
        abbr = abbreviation
      }
    )
  )
}



#' Import HCV/HBV purchasing/dispensing data.
#'
#' @param filepath The location of the file you wish to import. File must be a 
#' .xls or .xlsx file. File must contain a single sheet.
#'
#' @return A data.frame
#'
#' @examples
#' 
importdata <- function(filepath){
  as_tibble(
    read_excel(
      filepath,
      col_names = FALSE,
      sheet = 1
    )
  )
}


#' Adjusts difference
#'
#' @param old.data 
#' @param old.dataind 
#' @param old.datainddiff 
#'
#' @return
#'
#' @examples
#' 
diffadjust <- function(old.data, old.dataind, old.datainddiff){
  
  if(length(old.dataind)!=1){  
    if(nrow(old.data) < old.dataind[length(old.dataind)]){
      old.datainddiff[(length(old.datainddiff)+1)] = (nrow(old.data) - old.dataind[(length(old.dataind))] + 1)
    } else {
      x <- nrow(old.data)-sum(old.datainddiff)
      old.datainddiff[length(old.datainddiff)+1] <-  x
    }
  }
  return(old.datainddiff)
  }
