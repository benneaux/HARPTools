require(RODBC)
require(data.table)

startdate <- "'07/01/2016'" # Note: Dates must be in wacky US format
enddate <- "'09/30/2016'"  # i.e mm/dd/yyyy

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
Min(tblResultLab.ResultCode) AS MinOfResultCode, 
Min(tblCodeLabResult.Result) AS MinOfResult1, 
Min(tblCodeLabTest.Description) AS MinOfDescription,
Min(tblCodeLabResult.Colour) AS MinOfColour,
Min(tblCodeColours.Priority) AS MinOfPriority, 
FLOOR((CONVERT(int, [DOB])-CONVERT(int,tblInvestigationsRequest.[VisitDate]))/-365.25) AS Age, 
tblCodeLabGroup.LabGroupCode,
tblVisit.SWLast, 
tblVisit.IDULast, 
tblVisit.PartnersLast, 
tblVisit.Condoms, 
tblHIVPOCT.HIVEverTested, 
tblHIVPOCT.HIVLastTested, 
Min(tblHIVPOCT.HIVLastTestType) AS MinOfHIVLastTestType, 
Min(tblHIVPOCT.HIVWhereTested) AS MinOfHIVWhereTested, 
Min(tblHIVPOCT.HIVWhereTestedLocation) AS MinOfHIVWhereTestedLocation, 
Min(tblHIVPOCT.HIVTestWantedToday) AS MinOfHIVTestWantedToday, 
Min(tblHIVPOCT.SexWithPartnerType) AS MinOfSexWithPartnerType, 
Min(tblHIVPOCT.SWEver) AS MinOfSWEver, 
Min(tblHIVPOCT.SWLast) AS MinOfSWLast, 
Min(tblHIVPOCT.IDUEver) AS MinOfIDUEver, 
Min(tblHIVPOCT.IDULast) AS MinOfIDULast, 
Min(tblHIVPOCT.PartnersMaleLast3) AS MinOfPartnersMaleLast3, 
Min(tblHIVPOCT.PartnersMaleLast12) AS MinOfPartnersMaleLast12, 
Min(tblHIVPOCT.PartnersFemaleLast3) AS MinOfPartnersFemaleLast3, 
Min(tblHIVPOCT.PartnersFemaleLast12) AS MinOfPartnersFemaleLast12, 
Min(tblHIVPOCT.HIVStatusRegularPartner) AS MinOfHIVStatusRegularPartner, 
Min(tblHIVPOCT.CondomRegularMaleAnal) AS MinOfCondomRegularMaleAnal, 
Min(tblHIVPOCT.CondomCasualMaleAnal) AS MinOfCondomCasualMaleAnal, 
Min(tblHIVPOCT.CondomRegularFemale) AS MinOfCondomRegularFemale, 
Min(tblHIVPOCT.CondomCasualFemale) AS MinOfCondomCasualFemale, 
Min(tblHIVPOCT.HepCDiag) AS MinOfHepCDiag, 
Min(tblHIVPOCT.HepBVacc) AS MinOfHepBVacc, 
Min(tblHIVPOCT.SyphilisDiag) AS MinOfSyphilisDiag" 
                
query_from <- "FROM (tblVisit 
INNER JOIN (((((((tblInvestigationsRequest 
LEFT JOIN tblResultLab ON (tblInvestigationsRequest.LabGroupCode = tblResultLab.[Group]) 
AND (tblInvestigationsRequest.VisitDate = tblResultLab.VisitDate) 
AND (tblInvestigationsRequest.URNO = tblResultLab.URNO)) 
LEFT JOIN tblCodeLabResult ON tblResultLab.ResultCode = tblCodeLabResult.Code) 
LEFT JOIN tblCodeLabTest ON tblResultLab.Test = tblCodeLabTest.TestCode) 
INNER JOIN tblClient ON tblInvestigationsRequest.URNO = tblClient.URNO) 
INNER JOIN tblCodeClinic ON tblClient.Clinic = tblCodeClinic.ClinicNumber) 
LEFT JOIN tblCodeColours ON tblCodeLabResult.Colour = tblCodeColours.ColourCode) 
INNER JOIN tblCodeLabGroup ON tblInvestigationsRequest.LabGroupCode = tblCodeLabGroup.LabGroupCode) ON (tblInvestigationsRequest.VisitType = tblVisit.VisitType) 
AND (tblInvestigationsRequest.VisitDate = tblVisit.VisitDate) 
AND (tblVisit.URNO = tblInvestigationsRequest.URNO)) 
LEFT JOIN tblHIVPOCT ON (tblInvestigationsRequest.URNO = tblHIVPOCT.URNO) 
AND (tblInvestigationsRequest.VisitDate = tblHIVPOCT.VisitDate)"

query_groupby <- "GROUP BY tblInvestigationsRequest.URNO, 
tblClient.Clinic, 
tblCodeClinic.CentreCode,
tblClient.Sex,
tblClient.DOB, 
tblClient.Indigenous, 
tblClient.SW, 
tblClient.IDU, 
tblClient.Partners, 
tblInvestigationsRequest.VisitDate, 
FLOOR((CONVERT(int, [DOB])-CONVERT(int,tblInvestigationsRequest.[VisitDate]))/-365.25), 
tblCodeLabGroup.LabGroupCode,
tblVisit.SWLast, 
tblVisit.IDULast,
tblVisit.PartnersLast, 
tblVisit.Condoms, 
tblHIVPOCT.HIVEverTested, 
tblHIVPOCT.HIVLastTested"

query_having <- paste("HAVING (tblInvestigationsRequest.VisitDate BETWEEN", startdate, "AND", enddate,") 
AND (tblCodeLabGroup.LabGroupCode = 12) 
OR (tblInvestigationsRequest.VisitDate BETWEEN", startdate, "AND", enddate,") 
AND (tblCodeLabGroup.LabGroupCode = 101)",sep=" ")
 


# SQL====

conn <- odbcConnect("SHIPHNE")

query <- paste(query_select,query_from, query_groupby,query_having, ";",sep = " ")

data <- as.data.frame(sqlQuery(conn,query))


query_text <- as.data.frame(query)

odbcCloseAll()

# Calculations

data_lab <- subset(data,LabGroupCode==12)
data_poct <- subset(data,LabGroupCode==101)

#LabTest====

  ##Age====

lab_age_01 <- c(
  nrow(subset(data_lab, (is.na(Age)) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (is.na(Age)) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (is.na(Age)))),
  nrow(subset(data_lab, (is.na(Age)) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (is.na(Age)) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (is.na(Age)) & MinOfColour==12)))

lab_age_02 <- c(
  nrow(subset(data_lab, (Age<15) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age<15) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age<15))),
  nrow(subset(data_lab, (Age<15) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age<15) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age<15) & MinOfColour==12)))

lab_age_03 <- c(
  nrow(subset(data_lab, (Age>=15 & Age<=19) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=15 & Age<=19) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=15 & Age<=19))),
  nrow(subset(data_lab, (Age>=15 & Age<=19) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=15 & Age<=19) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=15 & Age<=19) & MinOfColour==12)))

lab_age_04 <- c(
  nrow(subset(data_lab, (Age>=20 & Age<=24) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=20 & Age<=24) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=20 & Age<=24))),
  nrow(subset(data_lab, (Age>=20 & Age<=24) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=20 & Age<=24) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=20 & Age<=24) & MinOfColour==12)))

lab_age_05 <- c(
  nrow(subset(data_lab, (Age>=25 & Age<=29) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=25 & Age<=29) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=25 & Age<=29))),
  nrow(subset(data_lab, (Age>=25 & Age<=29) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=25 & Age<=29) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=25 & Age<=29) & MinOfColour==12)))

lab_age_06 <- c(
  nrow(subset(data_lab, (Age>=30 & Age<=34) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=30 & Age<=34) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=30 & Age<=34))),
  nrow(subset(data_lab, (Age>=30 & Age<=34) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=30 & Age<=34) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=30 & Age<=34) & MinOfColour==12)))

lab_age_07 <- c(
  nrow(subset(data_lab, (Age>=35 & Age<=39) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=35 & Age<=39) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=35 & Age<=39))),
  nrow(subset(data_lab, (Age>=35 & Age<=39) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=35 & Age<=39) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=35 & Age<=39) & MinOfColour==12)))

lab_age_08 <- c(
  nrow(subset(data_lab, (Age>=40 & Age<=44) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=40 & Age<=44) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=40 & Age<=44))),
  nrow(subset(data_lab, (Age>=40 & Age<=44) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=40 & Age<=44) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=40 & Age<=44) & MinOfColour==12)))

lab_age_09 <- c(
  nrow(subset(data_lab, (Age>=45 & Age<=49) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=45 & Age<=49) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=45 & Age<=49))),
  nrow(subset(data_lab, (Age>=45 & Age<=49) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=45 & Age<=49) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=45 & Age<=49) & MinOfColour==12)))

lab_age_10 <- c(
  nrow(subset(data_lab, (Age>=50 & Age<=54) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=50 & Age<=54) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=50 & Age<=54))),
  nrow(subset(data_lab, (Age>=50 & Age<=54) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=50 & Age<=54) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=50 & Age<=54) & MinOfColour==12)))

lab_age_11 <- c(
  nrow(subset(data_lab, (Age>=55 & Age<=59) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=55 & Age<=59) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=55 & Age<=59))),
  nrow(subset(data_lab, (Age>=55 & Age<=59) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=55 & Age<=59) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=55 & Age<=59) & MinOfColour==12)))

lab_age_12 <- c(
  nrow(subset(data_lab, (Age>=60 & Age<=64) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=60 & Age<=64) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=60 & Age<=64))),
  nrow(subset(data_lab, (Age>=60 & Age<=64) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=60 & Age<=64) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=60 & Age<=64) & MinOfColour==12)))                

lab_age_13 <- c(
  nrow(subset(data_lab, (Age>=65) & (Sex=="M" | Sex=="m"))),
  nrow(subset(data_lab, (Age>=65) & (Sex=="F" | Sex=="f"))),
  nrow(subset(data_lab, (Age>=65))),
  nrow(subset(data_lab, (Age>=65) & (Sex=="M" | Sex=="m") & MinOfColour==12)),
  nrow(subset(data_lab, (Age>=65) & ((Sex=="F" | Sex=="f") & MinOfColour==12))),
  nrow(subset(data_lab, (Age>=65) & MinOfColour==12)))

lab_age <- rbind(
  lab_age_01,lab_age_02,lab_age_03,lab_age_04,lab_age_05,
  lab_age_06,lab_age_07,lab_age_08,lab_age_09,lab_age_10,
  lab_age_11,lab_age_12,lab_age_13)

rm(lab_age_01,lab_age_02,lab_age_03,lab_age_04,lab_age_05,lab_age_06,lab_age_07,
   lab_age_08,lab_age_09,lab_age_10,lab_age_11,lab_age_12,lab_age_13)

  ##ATSI====

lab_atsi_1 <- c(nrow(subset(data_lab, Indigenous == 1)),
                 nrow(subset(data_lab, Indigenous == 1 & MinOfColour == 12))                 )
lab_atsi_2 <- c(nrow(subset(data_lab, Indigenous == 2)),
                 nrow(subset(data_lab, Indigenous == 2 & MinOfColour == 12)))
lab_atsi_3 <- c(nrow(subset(data_lab, Indigenous == 3)),
                 nrow(subset(data_lab, Indigenous == 3 & MinOfColour == 12)))
lab_atsi_4 <- c(nrow(subset(data_lab, Indigenous == 4)),
                 nrow(subset(data_lab, Indigenous == 4 & MinOfColour == 12)))
lab_atsi_9 <- c(nrow(subset(data_lab, Indigenous == 9 | is.na(Indigenous))),
                 nrow(subset(data_lab, (Indigenous == 9 | is.na(Indigenous)) & MinOfColour==12)))

lab_atsi <- rbind(lab_atsi_1,
                  lab_atsi_2,
                  lab_atsi_3,
                  lab_atsi_4,
                  lab_atsi_9)

#POCT====
poct_prevtest <- nrow(subset(data_poct, HIVEverTested==1))

poct_syph <- c(nrow(subset(data_poct, MinOfSyphilisDiag==1)),
               nrow(subset(data_poct, MinOfSyphilisDiag==1 & MinOfColour==12)))
poct_hcv <- c(nrow(subset(data_poct,  MinOfHepCDiag==1)),
              nrow(subset(data_poct, MinOfHepCDiag==1 & MinOfColour==12)))

poct_diag <- rbind(poct_hcv,
                   poct_syph)

poct_atsi_1 <- c(nrow(subset(data_poct, Indigenous ==1)),
                 nrow(subset(data_poct, Indigenous ==1 & MinOfColour==12))                 )
poct_atsi_2 <- c(nrow(subset(data_poct, Indigenous ==2)),
                 nrow(subset(data_poct, Indigenous ==2 & MinOfColour==12)))
poct_atsi_3 <- c(nrow(subset(data_poct, Indigenous ==3)),
                 nrow(subset(data_poct, Indigenous ==3 & MinOfColour==12)))
poct_atsi_4 <- c(nrow(subset(data_poct, Indigenous ==4)),
                 nrow(subset(data_poct, Indigenous ==4 & MinOfColour==12)))
poct_atsi_9 <- c(nrow(subset(data_poct, Indigenous == 9 | is.na(Indigenous))),
                 nrow(subset(data_poct, (Indigenous == 9 | is.na(Indigenous)) & MinOfColour==12)))

poct_atsi <- rbind(poct_atsi_1,
                  poct_atsi_2,
                  poct_atsi_3,
                  poct_atsi_4,
                  poct_atsi_9)
