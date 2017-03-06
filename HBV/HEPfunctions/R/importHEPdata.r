#' Import HCV/HBV purchasing/dispensing data.
#'
#' @param filepath The location of the file you wish to import. File must be a 
#' .xls or .xlsx file. File must contain a single sheet.
#'
#' @return A data.frame
#'
#' @examples
#' 
importHEPdata <- function(filepath){
  data.table::as.data.table(
    readxl::read_excel(
      filepath,
      col_names = FALSE,
      sheet = 1
    )
  )
}