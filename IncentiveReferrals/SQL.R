# QueryDefs==================================================

query_select <- "SELECT tblInvestigationsRequest.URNO, tblClient.Clinic, tblCodeClinic.CentreCode, tblClient.Sex, tblClient.DOB, tblClient.Indigenous, tblClient.SW, tblClient.IDU, tblClient.Partners, tblInvestigationsRequest.VisitDate, Min(tblResultLab.ResultCode) AS ResultCode, Min(tblCodeLabResult.Result) AS Result, tblCodeLabGroup.LabGroupCode, tblCodeLabGroup.LabGroup, tblCodeLabTest.Test"

query_from <- "FROM (((((tblInvestigationsRequest LEFT JOIN tblResultLab ON (tblInvestigationsRequest.LabGroupCode = tblResultLab.[Group]) AND (tblInvestigationsRequest.VisitDate = tblResultLab.VisitDate) AND (tblInvestigationsRequest.URNO = tblResultLab.URNO)) LEFT JOIN tblCodeLabResult ON tblResultLab.ResultCode = tblCodeLabResult.Code) LEFT JOIN tblCodeLabTest ON tblResultLab.Test = tblCodeLabTest.TestCode) INNER JOIN tblClient ON tblInvestigationsRequest.URNO = tblClient.URNO) INNER JOIN tblCodeClinic ON tblClient.Clinic = tblCodeClinic.ClinicNumber) INNER JOIN tblCodeLabGroup ON tblInvestigationsRequest.LabGroupCode = tblCodeLabGroup.LabGroupCode"

query_groupby <- "GROUP BY tblInvestigationsRequest.URNO, tblClient.Clinic, tblCodeClinic.CentreCode, tblClient.Sex, tblClient.DOB, tblClient.Indigenous, tblClient.SW, tblClient.IDU, tblClient.Partners, tblInvestigationsRequest.VisitDate, tblCodeLabGroup.LabGroupCode, tblCodeLabGroup.LabGroup, tblCodeLabTest.Test"

query_having <- paste("HAVING (((tblInvestigationsRequest.VisitDate) Between", startdate,"And", enddate,") AND ((tblCodeLabGroup.LabGroupCode) In (1,5,7,8,12,21,73,96,102,144,145)));", sep = " ")

servicequery <- paste(query_select,
                      query_from,
                      query_groupby,
                      query_having,
                      sep = " ") 

# VisitQueryDefs==================================================

query_select <- "SELECT tblVisit.URNO, tblVisit.VisitDate, tblCodeVisitType.Type, tblCodeClinic.Clinic"

query_from <- "FROM (tblVisit INNER JOIN tblCodeClinic ON tblVisit.Clinic = tblCodeClinic.ClinicNumber) INNER JOIN tblCodeVisitType ON tblVisit.VisitType = tblCodeVisitType.TypeCode"
query_groupby <- "GROUP BY tblVisit.URNO, tblVisit.VisitDate, tblCodeVisitType.Type, tblCodeClinic.Clinic"
query_having <- paste("HAVING tblVisit.VisitDate Between", startdate,"And", enddate, sep = " ")

visitquery <- paste0(paste(query_select,
                      query_from,
                      query_groupby,
                      query_having,
                      sep = " "),
                     ";")

# SQL===================================================================

conn <- RODBC::odbcConnect("SHIPHNE")

servicedata <- as.data.frame(RODBC::sqlQuery(conn,servicequery))
visitdata <- as.data.frame(RODBC::sqlQuery(conn,visitquery))

RODBC::odbcCloseAll()

rm(servicequery,
   query_from,
   query_groupby,
   query_having,
   query_select,
   startdate,enddate
)