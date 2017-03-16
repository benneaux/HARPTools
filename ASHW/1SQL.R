
## ---- my-label ----

query <- paste0(paste("SELECT tblVisit.URNO, tblVisit.VisitDate, tblInvestigations.VisitNumber, tblInvestigations.VisitType
FROM (tblInvestigations 
  INNER JOIN (tblVisit INNER JOIN tblClient 
    ON tblVisit.URNO = tblClient.URNO) 
    ON (tblInvestigations.VisitType = tblVisit.VisitType) 
  AND (tblInvestigations.VisitDate = tblVisit.VisitDate) 
  AND (tblInvestigations.URNO = tblVisit.URNO)) 
  INNER JOIN tblCodeClinic 
    ON tblClient.Clinic = tblCodeClinic.ClinicNumber 
WHERE (((tblClient.Indigenous) In (1,2,3)) AND ((tblCodeClinic.Sector)=2))
GROUP BY tblVisit.URNO, tblVisit.VisitDate, tblInvestigations.VisitNumber, tblInvestigations.VisitType
HAVING tblVisit.VisitDate <", enddate),";")


conn <- RODBC::odbcConnect("SHIPHNE")

data <- as.data.frame(RODBC::sqlQuery(conn,query))


RODBC::odbcCloseAll()