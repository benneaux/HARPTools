require(data.table)
require(DT)

path <- "O:/HARP_Data/01 Reports/Scratch/"
file <- "tmpWebNAPMDSSummary.csv"
summary <- read.csv(paste0(path,file))
View(summary)
write.csv(summary, paste0(path,"WebNAPMDSSummary.csv"))

file <- "tmpWebNAPMDSServiceUnit.csv"
service <- read.csv(paste0(path,file))
View(service)
write.csv(service,  paste0(path,"WebNAPMDSServiceUnit.csv"))

file <- "tmpWebNAPMDSPatient.csv"
patient <- as.data.table(read.csv(paste0(path,file)))
View(patient)

patient <- patient[is.na(Post.Code), Post.Code:=9999]
patient <- patient[Post.Code==9999, Suburb:="UNUN9999"]
patient <- patient[Post.Code==9999, Street:="UNUN9999"]
patient <- patient[-which(is.na(patient$Service.Unit.Code)), ]

write.csv(patient,  paste0(path,"WebNAPMDSPatient.csv"))
