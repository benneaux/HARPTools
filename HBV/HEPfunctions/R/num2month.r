
#' Return the Month name or Month name abbreviation
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
    lubridate::month(
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