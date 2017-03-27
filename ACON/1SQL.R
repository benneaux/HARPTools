
library(RODBC)
## ---- my-label ----


query <- paste("SELECT tblVisit.URNO, tblVisit.VisitDate, tblClient.Sex, tblClient.Indigenous, tblVisit.PartnersLast, tblVisit.SWLast, tblInvestigationsRequest.LabGroupCode
           FROM tblClient 
              INNER JOIN (tblInvestigationsRequest RIGHT JOIN tblVisit 
                ON (tblInvestigationsRequest.VisitType = tblVisit.VisitType) 
                AND (tblInvestigationsRequest.VisitDate = tblVisit.VisitDate) 
                AND (tblInvestigationsRequest.URNO = tblVisit.URNO)) 
                  ON tblClient.URNO = tblVisit.URNO
           WHERE (((tblVisit.VisitDate) Between 42004 AND",enddate,") 
           AND ((tblVisit.Clinic)=2));")
                                   
#           AND ((tblInvestigationsRequest.LabGroupCode) In (12,96)) 
conn <- odbcConnect("SHIPHNE")

data <- as.data.frame(sqlQuery(conn,query))


odbcCloseAll()
rm(conn,query)