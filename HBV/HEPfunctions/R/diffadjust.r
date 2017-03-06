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
