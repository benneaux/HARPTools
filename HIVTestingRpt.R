# HIV Testing Report.

## Setup =======================================================================
rm(list =ls())
req.packages <- c("dplyr","tidyr","data.table",
                  "readxl","stringr","lubridate",
                  "stringi", "XLConnect","RODBC"
                  )

lapply(
  req.packages,
  require,
  character.only = TRUE,
  quietly = TRUE
)
rm(req.packages)
(.packages())
list(ls())

file.path <- "O:/HARP_Data/04 Templates/HIVSTI/" 

file.name <- paste("HIVTestingCD4Temp_Sept16.xlsx")
full.path <- paste(file.path,file.name, sep = "")
scratch.path <- "O:/HARP_Data/01 Reports/Scratch/"

con<-odbcConnectAccess("R:/ShipRptsCitrix.mdb")

template <- loadWorkbook(full.path, create = FALSE)
setStyleAction(template,XLC$"STYLE_ACTION.NONE")
agesex <- readNamedRegion(template, name = "ars")
names(agesex) <- c("AgeGroup","Male","Female","Total",
               "Positive(Male)",	"Positive(Female)",	"Total Positive")

hivnegtotal <- sqlQuery(con,"EXEC HIVTestingAgeGroup_AgeSexQ1")
hivnegtotal <- subset(hivnegtotal, select = c(-Code,-i,-u))
names(hivnegtotal) <- c("AgeGroup","Male","Female")
data <- agesex 

for(i in 1:nrow(data)) {
  if(hivnegtotal[i,"AgeGroup"] == data[i,"AgeGroup"]) {
    data[i,"Male"] = hivnegtotal[i,"Male"]
    data[i,"Female"] = hivnegtotal[i,"Female"]
  }
}


print(getForceFormulaRecalculation(template, sheet = "HIV Testing"))
writeNamedRegion(template, data[,2:3], name = "HIVNegAge", header = FALSE)

setForceFormulaRecalculation(template, sheet = "HIV Testing",TRUE)
saveWorkbook(template, paste(scratch.path,"test.xlsx",sep = ""))



close(con)

