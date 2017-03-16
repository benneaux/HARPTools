# Daniel requested information regarding the total number of HCV tests to use when showing
# proportions of tests offered to incentives clients.
# 
rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(dplyr)
require(XLConnect)
startdate <- "'11/01/2016'" # Note: Dates must be in wacky US format
enddate <- "'02/28/2017'"  # i.e mm/dd/yyyy

# QueryDefs==================================================

query_select <- "SELECT tblInvestigationsRequest.URNO, tblClient.Clinic, tblCodeClinic.CentreCode, tblClient.Sex, tblClient.DOB, tblClient.Indigenous, tblClient.SW, tblClient.IDU, tblClient.Partners, tblInvestigationsRequest.VisitDate, Min(tblResultLab.ResultCode) AS ResultCode, Min(tblCodeLabResult.Result) AS Result, tblCodeLabGroup.LabGroupCode, tblCodeLabGroup.LabGroup, tblCodeLabTest.Test"

query_from <- "FROM (((((tblInvestigationsRequest LEFT JOIN tblResultLab ON (tblInvestigationsRequest.LabGroupCode = tblResultLab.[Group]) AND (tblInvestigationsRequest.VisitDate = tblResultLab.VisitDate) AND (tblInvestigationsRequest.URNO = tblResultLab.URNO)) LEFT JOIN tblCodeLabResult ON tblResultLab.ResultCode = tblCodeLabResult.Code) LEFT JOIN tblCodeLabTest ON tblResultLab.Test = tblCodeLabTest.TestCode) INNER JOIN tblClient ON tblInvestigationsRequest.URNO = tblClient.URNO) INNER JOIN tblCodeClinic ON tblClient.Clinic = tblCodeClinic.ClinicNumber) INNER JOIN tblCodeLabGroup ON tblInvestigationsRequest.LabGroupCode = tblCodeLabGroup.LabGroupCode"

query_groupby <- "GROUP BY tblInvestigationsRequest.URNO, tblClient.Clinic, tblCodeClinic.CentreCode, tblClient.Sex, tblClient.DOB, tblClient.Indigenous, tblClient.SW, tblClient.IDU, tblClient.Partners, tblInvestigationsRequest.VisitDate, tblCodeLabGroup.LabGroupCode, tblCodeLabGroup.LabGroup, tblCodeLabTest.Test"

# Only looks for HCV tests (Group = 8).
# 
query_having <- paste("HAVING (((tblInvestigationsRequest.VisitDate) Between", startdate,"And", enddate,") AND ((tblCodeLabGroup.LabGroupCode) In (8)));", sep = " ")

servicequery <- paste(query_select,
                      query_from,
                      query_groupby,
                      query_having,
                      sep = " ") 

conn <- RODBC::odbcConnect("SHIPHNE")

servicedata <- as.data.frame(RODBC::sqlQuery(conn,servicequery))

RODBC::odbcCloseAll()

rm(servicequery,
   query_from,
   query_groupby,
   query_having,
   query_select,
   startdate,enddate
)

testdata <- servicedata %>%
  mutate(Site = case_when(servicedata$CentreCode == 32001 ~ "Tamworth",
                          servicedata$CentreCode == 31012 ~ "Newcastle",
                          servicedata$CentreCode == 33005 ~ "Taree",
                          servicedata$CentreCode == 32002 ~ "Tamworth Outreach",
                          servicedata$CentreCode == 32003 ~ "Sex Worker Outreach")) %>%
  mutate(ClinicName = case_when(servicedata$Clinic == 1 ~ "Pacific Clinic",
                                servicedata$Clinic == 2 ~ "ACON",
                                servicedata$Clinic == 11 ~ "Clinic 468",
                                servicedata$Clinic == 13 ~ "Tamworth Outreach",
                                servicedata$Clinic == 17 ~ "Taree SHC")) %>%
  select(-c(Clinic, CentreCode, Sex, DOB, Indigenous, SW, IDU, Partners))

output_name <- "Data/HCVTestingData_01Nov16-28Feb17.xlsx"


wb <- loadWorkbook (output_name , create = TRUE)
createSheet(wb, name = "HCVInvestigations")
writeWorksheet(wb, testdata, "HCVInvestigations")
saveWorkbook(wb)