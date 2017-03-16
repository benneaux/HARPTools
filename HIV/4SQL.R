# QueryDefs====

query_select <- "SELECT tblInvestigationsRequest.URNO,
  tblClient.Clinic, 
  tblCodeClinic.CentreCode,
  tblClient.Sex, 
  tblClient.DOB, 
  tblClient.Indigenous, 
  tblClient.SW, 
  tblClient.IDU, 
  tblClient.Partners, 
  tblInvestigationsRequest.VisitDate,
  tblResultLab.LabNumber,
  Min(tblResultLab.ResultCode) AS ResultCode, 
  Min(tblCodeLabResult.Result) AS Result, 
  Min(tblCodeLabTest.Description) AS Description,
  Min(tblCodeLabResult.Colour) AS Colour,
  Min(tblCodeColours.Priority) AS Priority, 
  FLOOR((CONVERT(int, [DOB])-CONVERT(int,tblInvestigationsRequest.[VisitDate]))/-365.25) AS Age, 
  tblCodeLabGroup.LabGroupCode,
  tblHIVPOCT.QuestionnaireDateTime,
  tblHIVPOCT.HIVEverTested, 
  tblHIVPOCT.HIVLastTested, 
  Min(tblHIVPOCT.HIVLastTestType) AS HIVLastTestType, 
  Min(tblHIVPOCT.HIVWhereTested) AS HIVWhereTested, 
  Min(tblHIVPOCT.HIVWhereTestedLocation) AS HIVWhereTestedLocation, 
  Min(tblHIVPOCT.HIVTestWantedToday) AS HIVTestWantedToday, 
  Min(tblHIVPOCT.SexWithPartnerType) AS SexWithPartnerType, 
  Min(tblHIVPOCT.PartnersMaleLast3) AS PartnersMaleLast3, 
  Min(tblHIVPOCT.PartnersMaleLast12) AS PartnersMaleLast12, 
  Min(tblHIVPOCT.PartnersFemaleLast3) AS PartnersFemaleLast3, 
  Min(tblHIVPOCT.PartnersFemaleLast12) AS PartnersFemaleLast12, 
  Min(tblHIVPOCT.HIVStatusRegularPartner) AS HIVStatusRegularPartner, 
  Min(tblHIVPOCT.CondomRegularMaleAnal) AS CondomRegularMaleAnal, 
  Min(tblHIVPOCT.CondomCasualMaleAnal) AS CondomCasualMaleAnal, 
  Min(tblHIVPOCT.CondomRegularFemale) AS CondomRegularFemale, 
  Min(tblHIVPOCT.CondomCasualFemale) AS CondomCasualFemale, 
  Min(tblHIVPOCT.HepCDiag) AS HepCDiag, 
  Min(tblHIVPOCT.HepBVacc) AS HepBVacc, 
  Min(tblHIVPOCT.SyphilisDiag) AS SyphilisDiag"

query_from <- "FROM (((((((tblInvestigationsRequest LEFT JOIN tblResultLab ON (tblInvestigationsRequest.URNO = tblResultLab.URNO) AND (tblInvestigationsRequest.VisitDate = tblResultLab.VisitDate) AND (tblInvestigationsRequest.LabGroupCode = tblResultLab.[Group])) 
  LEFT JOIN tblCodeLabResult ON tblResultLab.ResultCode = tblCodeLabResult.Code) 
  LEFT JOIN tblCodeLabTest ON tblResultLab.Test = tblCodeLabTest.TestCode)
  INNER JOIN tblClient ON tblInvestigationsRequest.URNO = tblClient.URNO) 
  INNER JOIN tblCodeClinic ON tblClient.Clinic = tblCodeClinic.ClinicNumber) 
  LEFT JOIN tblCodeColours ON tblCodeLabResult.Colour = tblCodeColours.ColourCode) 
  INNER JOIN tblCodeLabGroup ON tblInvestigationsRequest.LabGroupCode = tblCodeLabGroup.LabGroupCode) 
  LEFT JOIN tblHIVPOCT ON (tblInvestigationsRequest.VisitDate = tblHIVPOCT.VisitDate) AND (tblInvestigationsRequest.URNO = tblHIVPOCT.URNO)"

query_groupby <- "GROUP BY tblInvestigationsRequest.URNO, 
  tblClient.Clinic,
  tblCodeClinic.CentreCode, 
  tblClient.Sex, tblClient.DOB, 
  tblClient.Indigenous, tblClient.SW, 
  tblClient.IDU, tblClient.Partners, 
  tblInvestigationsRequest.VisitDate,
  tblResultLab.LabNumber,
  tblCodeLabGroup.LabGroupCode,
  tblHIVPOCT.QuestionnaireDateTime,
  tblHIVPOCT.HIVEverTested,
  tblHIVPOCT.HIVLastTested,
  FLOOR((CONVERT(int, [DOB])-CONVERT(int,tblInvestigationsRequest.[VisitDate]))/-365.25)"

query_having <- paste("HAVING (tblInvestigationsRequest.[VisitDate] BETWEEN",
                      startdate,
                      "AND",
                      enddate,
                      ") AND (tblCodeLabGroup.LabGroupCode = 12) OR (tblInvestigationsRequest.[VisitDate] BETWEEN",
                      startdate,
                      "AND",
                      enddate,
                      ") AND (tblCodeLabGroup.LabGroupCode = 96)"
                      )

servicequery <- paste(query_select,
                   query_from,
                   query_groupby,
                   query_having, 
                   ";"
                   ) 

query_select <- "SELECT tblVisit.URNO, 
  tblVisit.VisitDate,
  tblVisit.SWLast,
  tblVisit.IDULast,
  tblVisit.PartnersLast,
  tblVisit.Condoms" 

query_from <- "FROM  tblVisit"

query_groupby <- "GROUP BY tblVisit.URNO,
  tblVisit.VisitDate,
  tblVisit.SWLast,
  tblVisit.IDULast,
  tblVisit.PartnersLast,
  tblVisit.Condoms"

query_having <- paste("HAVING (tblVisit.VisitDate BETWEEN",
                      startdate,
                      "AND", 
                      enddate,
                      ")"
                      )

visitquery <- paste(query_select,
                    query_from, 
                    query_groupby,
                    query_having,
                    ";",
                    sep = " "
                    )

# SQL====

conn <- odbcConnect("SHIPHNE")

servicedata <- as.data.frame(sqlQuery(conn,servicequery))
visitdata <- as.data.frame(sqlQuery(conn, visitquery))
odbcCloseAll()

rm(servicequery,
   visitquery,
   query_from,
   query_groupby,
   query_having,
   query_select,
   startdate,
   enddate
   )