

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

importdata <- function(filepath){
  data.table::as.data.table(
    readxl::read_excel(
      filepath,
      col_names = FALSE,
      sheet = 1
    )
  )
}

diffadjust <- function(data,dataind,datainddiff,offset){
  if(length(data)<dataind[length(dataind)]){
    datainddiff[length(datainddiff)+1] <- nrow(data)-dataind[length(dataind)] + 1
  } else {
    datainddiff[length(datainddiff)+1] <- nrow(data)-dataind[length(dataind) - 1] + as.numeric(offset)
  }
  return(data)
  return(dataind)
  return(datainddiff)
}