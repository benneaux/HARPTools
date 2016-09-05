require(data.table)
require(DT)

path <- "O:\\HARP_Data\\tmpWebNAPMDSSummary.csv"
summary <- read.csv(path)
summary <- summary[6:nrow(summary),]
write.csv(summary, "WebNAPMDSSummary.csv")

path <- "O:\\HARP_Data\\tmpWebNAPMDSServiceUnit.csv"
service <- read.csv(path)
View(service)
service <- service[6:nrow(service),]
write.csv(service, "WebNAPMDSServiceUnit.csv")

path <- "O:\\HARP_Data\\tmpWebNAPMDSPatient.csv"
patient <- as.data.table(read.csv(path))
View(patient)

patient[is.na(Post.Code), Post.Code:=9999]
patient[Post.Code==9999, Suburb:="UNUN9999"]
patient[Post.Code==9999, Street:="UNUN9999"]
patient <- patient[-which(is.na(patient$Service.Unit.Code)), ]

write.csv(patient, "WebNAPMDSPatient.csv")
