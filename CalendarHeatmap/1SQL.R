startdate <- "'01/01/2014'"
enddate <- "'03/23/2017'"
## ---- my-label ----

query <- paste0(
paste(
  "SELECT tblServiceActivities.URNO,
          tblServiceActivities.VisitDate, 
          tblServiceActivities.MainService, 
          tblCodeClinic.Clinic
  FROM (tblServiceActivities 
    INNER JOIN tblVisit 
                ON  (tblServiceActivities.VisitType = tblVisit.VisitType) 
                AND (tblServiceActivities.VisitDate = tblVisit.VisitDate) 
                AND (tblServiceActivities.URNO = tblVisit.URNO))
    INNER JOIN tblCodeClinic 
                ON tblVisit.Clinic = tblCodeClinic.ClinicNumber
  GROUP BY tblServiceActivities.URNO, 
           tblServiceActivities.VisitDate, 
           tblServiceActivities.MainService, 
           tblCodeClinic.Clinic
  HAVING (((tblServiceActivities.VisitDate) Between", startdate, "AND", enddate),"));")


conn <- RODBC::odbcConnect("SHIPHNE")

data <- as.data.frame(RODBC::sqlQuery(conn,query))


RODBC::odbcCloseAll()