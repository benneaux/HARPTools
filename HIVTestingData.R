
HIVTestingData <- function(startdate, enddate) {
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
  Min(tblResultLab.ResultCode) AS ResultCode, 
  Min(tblCodeLabResult.Result) AS Result, 
  Min(tblCodeLabTest.Description) AS Description,
  Min(tblCodeLabResult.Colour) AS Colour,
  Min(tblCodeColours.Priority) AS Priority, 
  FLOOR((CONVERT(int, [DOB])-CONVERT(int,tblInvestigationsRequest.[VisitDate]))/-365.25) AS Age, 
  tblCodeLabGroup.LabGroupCode,
  tblVisit.SWLast, 
  tblVisit.IDULast, 
  tblVisit.PartnersLast, 
  tblVisit.Condoms, 
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
  AND (tblCodeLabGroup.LabGroupCode = 96)",sep=" ")
   
  
  
  # SQL====

  conn <- odbcConnect("SHIPHNE")
  query <- paste(query_select,query_from, query_groupby,query_having, ";",sep = " ")
  data <- as.data.frame(sqlQuery(conn,query))
  odbcCloseAll()
  
  rm(query,query_from,query_groupby,query_having,query_select,startdate,enddate)
  
  # Calculations====
  data$Sex <- str_to_upper(data$Sex)
  data$Partners <- str_to_upper(data$Partners)
  data$PartnersLast <- str_to_upper(data$PartnersLast)

  data[is.na(data$SW),"SW"] <- 9
  # data[data$SW == 2, "SW"] <- 3
  # data[data$SW == 1, "SW"] <- 2
  # data <- within(data, SW[SWLast == 1] <- 1)
  # data <- within(data, SW[SWLast == 3] <- 1)
  
  
  
  data_lab <<- subset(data,LabGroupCode==12)
  data_poct <<- subset(data,LabGroupCode==96)
  data_poct_male <- subset(data_poct,Sex=="M")
  data_poct_msm <- subset(data_poct,Sex=="M" & {PartnersLast=="S" | PartnersLast=="B"})
  # LabTest====
    ## Age====
  
  lab_age_01 <- c(
    nrow(subset(data_lab, (is.na(Age)) & Sex=="M")),
    nrow(subset(data_lab, (is.na(Age)) & Sex=="F" )),
    nrow(subset(data_lab, (is.na(Age)))),
    nrow(subset(data_lab, (is.na(Age)) & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, (is.na(Age)) & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, (is.na(Age)) & Colour==12)))
  
  lab_age_02 <- c(
    nrow(subset(data_lab, {Age<15} & Sex=="M" )),
    nrow(subset(data_lab, {Age<15} & Sex=="F" )),
    nrow(subset(data_lab, {Age<15})),
    nrow(subset(data_lab, {Age<15} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age<15} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age<15} & Colour==12)))
  
  lab_age_03 <- c(
    nrow(subset(data_lab, {Age>=15 & Age<=19} & Sex=="M")),
    nrow(subset(data_lab, {Age>=15 & Age<=19} & Sex=="F")),
    nrow(subset(data_lab, {Age>=15 & Age<=19})),
    nrow(subset(data_lab, {Age>=15 & Age<=19} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=15 & Age<=19} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=15 & Age<=19} & Colour==12)))
  
  lab_age_04 <- c(
    nrow(subset(data_lab, {Age>=20 & Age<=24} & Sex=="M")),
    nrow(subset(data_lab, {Age>=20 & Age<=24} & Sex=="F")),
    nrow(subset(data_lab, {Age>=20 & Age<=24})),
    nrow(subset(data_lab, {Age>=20 & Age<=24} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=20 & Age<=24} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=20 & Age<=24} & Colour==12)))
  
  lab_age_05 <- c(
    nrow(subset(data_lab, {Age>=25 & Age<=29} & Sex=="M")),
    nrow(subset(data_lab, {Age>=25 & Age<=29} & Sex=="F")),
    nrow(subset(data_lab, {Age>=25 & Age<=29})),
    nrow(subset(data_lab, {Age>=25 & Age<=29} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=25 & Age<=29} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=25 & Age<=29} & Colour==12)))
  
  lab_age_06 <- c(
    nrow(subset(data_lab, {Age>=30 & Age<=34} & Sex=="M")),
    nrow(subset(data_lab, {Age>=30 & Age<=34} & Sex=="F")),
    nrow(subset(data_lab, {Age>=30 & Age<=34})),
    nrow(subset(data_lab, {Age>=30 & Age<=34} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=30 & Age<=34} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=30 & Age<=34} & Colour==12)))
  
  lab_age_07 <- c(
    nrow(subset(data_lab, {Age>=35 & Age<=39} & Sex=="M")),
    nrow(subset(data_lab, {Age>=35 & Age<=39} & Sex=="F")),
    nrow(subset(data_lab, {Age>=35 & Age<=39})),
    nrow(subset(data_lab, {Age>=35 & Age<=39} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=35 & Age<=39} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=35 & Age<=39} & Colour==12)))
  
  lab_age_08 <- c(
    nrow(subset(data_lab, {Age>=40 & Age<=44} & Sex=="M")),
    nrow(subset(data_lab, {Age>=40 & Age<=44} & Sex=="F")),
    nrow(subset(data_lab, {Age>=40 & Age<=44})),
    nrow(subset(data_lab, {Age>=40 & Age<=44} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=40 & Age<=44} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=40 & Age<=44} & Colour==12)))
  
  lab_age_09 <- c(
    nrow(subset(data_lab, {Age>=45 & Age<=49} & Sex=="M")),
    nrow(subset(data_lab, {Age>=45 & Age<=49} & Sex=="F")),
    nrow(subset(data_lab, {Age>=45 & Age<=49})),
    nrow(subset(data_lab, {Age>=45 & Age<=49} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=45 & Age<=49} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=45 & Age<=49} & Colour==12)))
  
  lab_age_10 <- c(
    nrow(subset(data_lab, {Age>=50 & Age<=54} & Sex=="M")),
    nrow(subset(data_lab, {Age>=50 & Age<=54} & Sex=="F")),
    nrow(subset(data_lab, {Age>=50 & Age<=54})),
    nrow(subset(data_lab, {Age>=50 & Age<=54} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=50 & Age<=54} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=50 & Age<=54} & Colour==12)))
  
  lab_age_11 <- c(
    nrow(subset(data_lab, {Age>=55 & Age<=59} & Sex=="M")),
    nrow(subset(data_lab, {Age>=55 & Age<=59} & Sex=="F")),
    nrow(subset(data_lab, {Age>=55 & Age<=59})),
    nrow(subset(data_lab, {Age>=55 & Age<=59} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=55 & Age<=59} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=55 & Age<=59} & Colour==12)))
  
  lab_age_12 <- c(
    nrow(subset(data_lab, {Age>=60 & Age<=64} & Sex=="M")),
    nrow(subset(data_lab, {Age>=60 & Age<=64} & Sex=="F")),
    nrow(subset(data_lab, {Age>=60 & Age<=64})),
    nrow(subset(data_lab, {Age>=60 & Age<=64} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=60 & Age<=64} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=60 & Age<=64} & Colour==12)))                
  
  lab_age_13 <- c(
    nrow(subset(data_lab, {Age>=65} & Sex=="M")),
    nrow(subset(data_lab, {Age>=65} & Sex=="F")),
    nrow(subset(data_lab, {Age>=65})),
    nrow(subset(data_lab, {Age>=65} & Sex=="M" & Colour==12)),
    nrow(subset(data_lab, {Age>=65} & Sex=="F" & Colour==12)),
    nrow(subset(data_lab, {Age>=65} & Colour==12)))
  
  lab_age <<- rbind(
    lab_age_01,lab_age_02,lab_age_03,lab_age_04,lab_age_05,
    lab_age_06,lab_age_07,lab_age_08,lab_age_09,lab_age_10,
    lab_age_11,lab_age_12,lab_age_13)
  
  rm(lab_age_01,lab_age_02,lab_age_03,lab_age_04,lab_age_05,lab_age_06,lab_age_07,
     lab_age_08,lab_age_09,lab_age_10,lab_age_11,lab_age_12,lab_age_13)
  
    ## ATSI====
  
    lab_atsi_1 <- c(nrow(subset(data_lab, Indigenous == 1)),
                     nrow(subset(data_lab, Indigenous == 1 & Colour == 12)))
    lab_atsi_2 <- c(nrow(subset(data_lab, Indigenous == 2)),
                     nrow(subset(data_lab, Indigenous == 2 & Colour == 12)))
    lab_atsi_3 <- c(nrow(subset(data_lab, Indigenous == 3)),
                     nrow(subset(data_lab, Indigenous == 3 & Colour == 12)))
    lab_atsi_4 <- c(nrow(subset(data_lab, Indigenous == 4)),
                     nrow(subset(data_lab, Indigenous == 4 & Colour == 12)))
    lab_atsi_9 <- c(nrow(subset(data_lab, Indigenous == 9 | is.na(Indigenous))),
                     nrow(subset(data_lab, {Indigenous == 9 | is.na(Indigenous)} & Colour==12)))
    
    lab_atsi <<- rbind(lab_atsi_1,lab_atsi_2,lab_atsi_3,lab_atsi_4,lab_atsi_9)
    rm(lab_atsi_1,lab_atsi_2,lab_atsi_3,lab_atsi_4,lab_atsi_9)
  
    ## IDU====
    lab_idu_1 <- c(nrow(subset(data_lab, IDULast == 1)),
                  nrow(subset(data_lab, IDULast == 1 & Colour==12))                 )
    lab_idu_2 <- c(nrow(subset(data_lab, IDULast == 2)),
                  nrow(subset(data_lab, IDULast == 2 & Colour==12)))
    lab_idu_3 <- c(nrow(subset(data_lab, IDULast == 3)),
                  nrow(subset(data_lab, IDULast == 3 & Colour==12)))
    lab_idu_9 <- c(nrow(subset(data_lab, IDULast == 9 | is.na(IDULast))),
                  nrow(subset(data_lab, {IDULast == 9 | is.na(IDULast)} & Colour==12)))
    
    lab_idu <<- rbind(lab_idu_1,lab_idu_2,lab_idu_3,lab_idu_9)
    rm(lab_idu_1,lab_idu_2,lab_idu_3,lab_idu_9)
    
    ## Risk====
    lab_risk_1 <- c(
      nrow(subset(data_lab, PartnersLast== "O" & Sex=="M")),
      nrow(subset(data_lab, PartnersLast== "O"& Sex=="F")),
      nrow(subset(data_lab, PartnersLast== "O")),
      nrow(subset(data_lab, PartnersLast== "O" & Sex=="M" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "O"& Sex=="F" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "O" & Colour == 12)))
    lab_risk_2 <- c(
      nrow(subset(data_lab, PartnersLast== "B" & Sex=="M")),
      nrow(subset(data_lab, PartnersLast== "B"& Sex=="F")),
      nrow(subset(data_lab, PartnersLast== "B")),
      nrow(subset(data_lab, PartnersLast== "B" & Sex=="M" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "B"& Sex=="F" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "B" & Colour == 12)))
    lab_risk_3 <- c(
      nrow(subset(data_lab, PartnersLast== "S" & Sex=="M")),
      nrow(subset(data_lab, PartnersLast== "S"& Sex=="F")),
      nrow(subset(data_lab, PartnersLast== "S")),
      nrow(subset(data_lab, PartnersLast== "S" & Sex=="M" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "S"& Sex=="F" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "S" & Colour == 12)))
    lab_risk_4 <- c(
      nrow(subset(data_lab, PartnersLast== "N" & Sex=="M")),
      nrow(subset(data_lab, PartnersLast== "N"& Sex=="F")),
      nrow(subset(data_lab, PartnersLast== "N")),
      nrow(subset(data_lab, PartnersLast== "N" & Sex=="M" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "N"& Sex=="F" & Colour == 12)),
      nrow(subset(data_lab, PartnersLast== "N" & Colour == 12)))
    lab_risk_5 <- c(
      nrow(subset(data_lab, is.na(PartnersLast) & Sex=="M")),
      nrow(subset(data_lab, is.na(PartnersLast) & Sex=="F")),
      nrow(subset(data_lab, is.na(PartnersLast))),
      nrow(subset(data_lab, is.na(PartnersLast) & Sex=="M" & Colour == 12)),
      nrow(subset(data_lab, is.na(PartnersLast) & Sex=="F" & Colour == 12)),
      nrow(subset(data_lab, is.na(PartnersLast) & Colour == 12)))
    
    lab_risk <<- rbind(lab_risk_1,lab_risk_2,lab_risk_3,lab_risk_4,lab_risk_5)
    rm(lab_risk_1,lab_risk_2,lab_risk_3,lab_risk_4,lab_risk_5)
    
    ## SW====
    lab_sw_1 <- c(nrow(subset(data_lab, SWLast == 1)),
                   nrow(subset(data_lab, SWLast == 1 & Colour==12))                 )
    lab_sw_2 <- c(nrow(subset(data_lab, SWLast == 2)),
                   nrow(subset(data_lab, SWLast == 2 & Colour==12)))
    lab_sw_3 <- c(nrow(subset(data_lab, SWLast == 3)),
                   nrow(subset(data_lab, SWLast == 3 & Colour==12)))
    lab_sw_9 <- c(nrow(subset(data_lab, SWLast == 9 | is.na(SWLast))),
                   nrow(subset(data_lab, {SWLast == 9 | is.na(SWLast)} & Colour==12)))
    
    lab_sw <<- rbind(lab_sw_1,lab_sw_2,lab_sw_3,lab_sw_9)
    rm(lab_sw_1,lab_sw_2,lab_sw_3,lab_sw_9)
  
    
  # POCT====
    ## Age====
    poct_age_01 <- c(
      nrow(subset(data_poct, (is.na(Age)) & Sex=="M")),
      nrow(subset(data_poct, (is.na(Age)) & Sex=="F")),
      nrow(subset(data_poct, (is.na(Age)))),
      nrow(subset(data_poct, (is.na(Age)) & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, (is.na(Age)) & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, (is.na(Age)) & Colour==12)))
    
    poct_age_02 <- c(
      nrow(subset(data_poct, {Age<15} & Sex=="M")),
      nrow(subset(data_poct, {Age<15} & Sex=="F")),
      nrow(subset(data_poct, {Age<15})),
      nrow(subset(data_poct, {Age<15} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age<15} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age<15} & Colour==12)))
    
    poct_age_03 <- c(
      nrow(subset(data_poct, {Age>=15 & Age<=19} & Sex=="M")),
      nrow(subset(data_poct, {Age>=15 & Age<=19} & Sex=="F")),
      nrow(subset(data_poct, {Age>=15 & Age<=19})),
      nrow(subset(data_poct, {Age>=15 & Age<=19} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=15 & Age<=19} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=15 & Age<=19} & Colour==12)))
    
    poct_age_04 <- c(
      nrow(subset(data_poct, {Age>=20 & Age<=24} & Sex=="M")),
      nrow(subset(data_poct, {Age>=20 & Age<=24} & Sex=="F")),
      nrow(subset(data_poct, {Age>=20 & Age<=24})),
      nrow(subset(data_poct, {Age>=20 & Age<=24} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=20 & Age<=24} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=20 & Age<=24} & Colour==12)))
    
    poct_age_05 <- c(
      nrow(subset(data_poct, {Age>=25 & Age<=29} & Sex=="M")),
      nrow(subset(data_poct, {Age>=25 & Age<=29} & Sex=="F")),
      nrow(subset(data_poct, {Age>=25 & Age<=29})),
      nrow(subset(data_poct, {Age>=25 & Age<=29} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=25 & Age<=29} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=25 & Age<=29} & Colour==12)))
    
    poct_age_06 <- c(
      nrow(subset(data_poct, {Age>=30 & Age<=34} & Sex=="M")),
      nrow(subset(data_poct, {Age>=30 & Age<=34} & Sex=="F")),
      nrow(subset(data_poct, {Age>=30 & Age<=34})),
      nrow(subset(data_poct, {Age>=30 & Age<=34} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=30 & Age<=34} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=30 & Age<=34} & Colour==12)))
    
    poct_age_07 <- c(
      nrow(subset(data_poct, {Age>=35 & Age<=39} & Sex=="M")),
      nrow(subset(data_poct, {Age>=35 & Age<=39} & Sex=="F")),
      nrow(subset(data_poct, {Age>=35 & Age<=39})),
      nrow(subset(data_poct, {Age>=35 & Age<=39} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=35 & Age<=39} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=35 & Age<=39} & Colour==12)))
    
    poct_age_08 <- c(
      nrow(subset(data_poct, {Age>=40 & Age<=44} & Sex=="M")),
      nrow(subset(data_poct, {Age>=40 & Age<=44} & Sex=="F")),
      nrow(subset(data_poct, {Age>=40 & Age<=44})),
      nrow(subset(data_poct, {Age>=40 & Age<=44} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=40 & Age<=44} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=40 & Age<=44} & Colour==12)))
    
    poct_age_09 <- c(
      nrow(subset(data_poct, {Age>=45 & Age<=49} & Sex=="M")),
      nrow(subset(data_poct, {Age>=45 & Age<=49} & Sex=="F")),
      nrow(subset(data_poct, {Age>=45 & Age<=49})),
      nrow(subset(data_poct, {Age>=45 & Age<=49} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=45 & Age<=49} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=45 & Age<=49} & Colour==12)))
    
    poct_age_10 <- c(
      nrow(subset(data_poct, {Age>=50 & Age<=54} & Sex=="M")),
      nrow(subset(data_poct, {Age>=50 & Age<=54} & Sex=="F")),
      nrow(subset(data_poct, {Age>=50 & Age<=54})),
      nrow(subset(data_poct, {Age>=50 & Age<=54} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=50 & Age<=54} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=50 & Age<=54} & Colour==12)))
    
    poct_age_11 <- c(
      nrow(subset(data_poct, {Age>=55 & Age<=59} & Sex=="M")),
      nrow(subset(data_poct, {Age>=55 & Age<=59} & Sex=="F")),
      nrow(subset(data_poct, {Age>=55 & Age<=59})),
      nrow(subset(data_poct, {Age>=55 & Age<=59} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=55 & Age<=59} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=55 & Age<=59} & Colour==12)))
    
    poct_age_12 <- c(
      nrow(subset(data_poct, {Age>=60 & Age<=64} & Sex=="M")),
      nrow(subset(data_poct, {Age>=60 & Age<=64} & Sex=="F")),
      nrow(subset(data_poct, {Age>=60 & Age<=64})),
      nrow(subset(data_poct, {Age>=60 & Age<=64} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=60 & Age<=64} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=60 & Age<=64} & Colour==12)))                
    
    poct_age_13 <- c(
      nrow(subset(data_poct, {Age>=65} & Sex=="M")),
      nrow(subset(data_poct, {Age>=65} & Sex=="F")),
      nrow(subset(data_poct, {Age>=65})),
      nrow(subset(data_poct, {Age>=65} & Sex=="M" & Colour==12)),
      nrow(subset(data_poct, {Age>=65} & Sex=="F" & Colour==12)),
      nrow(subset(data_poct, {Age>=65} & Colour==12)))
    
    poct_age <<- rbind(
      poct_age_01,poct_age_02,poct_age_03,poct_age_04,poct_age_05,
      poct_age_06,poct_age_07,poct_age_08,poct_age_09,poct_age_10,
      poct_age_11,poct_age_12,poct_age_13)
    
    rm(poct_age_01,poct_age_02,poct_age_03,poct_age_04,poct_age_05,poct_age_06,poct_age_07,
       poct_age_08,poct_age_09,poct_age_10,poct_age_11,poct_age_12,poct_age_13)
  
    ## ATSI====
    poct_atsi_1 <- c(nrow(subset(data_poct, Indigenous == 1)),
                     nrow(subset(data_poct, Indigenous == 1 & Colour==12))                 )
    poct_atsi_2 <- c(nrow(subset(data_poct, Indigenous == 2)),
                     nrow(subset(data_poct, Indigenous == 2 & Colour==12)))
    poct_atsi_3 <- c(nrow(subset(data_poct, Indigenous == 3)),
                     nrow(subset(data_poct, Indigenous == 3 & Colour==12)))
    poct_atsi_4 <- c(nrow(subset(data_poct, Indigenous == 4)),
                     nrow(subset(data_poct, Indigenous ==4 & Colour==12)))
    poct_atsi_9 <- c(nrow(subset(data_poct, Indigenous == 9 | is.na(Indigenous))),
                     nrow(subset(data_poct, {Indigenous == 9 | is.na(Indigenous)} & Colour==12)))
    
    poct_atsi <<- rbind(poct_atsi_1,poct_atsi_2,poct_atsi_3,poct_atsi_4,poct_atsi_9)
    rm(poct_atsi_1,poct_atsi_2,poct_atsi_3,poct_atsi_4,poct_atsi_9)
    
    ## IDU====
    poct_idu_1 <- c(nrow(subset(data_poct, IDULast == 1)),
                   nrow(subset(data_poct, IDULast == 1 & Colour==12))                 )
    poct_idu_2 <- c(nrow(subset(data_poct, IDULast == 2)),
                   nrow(subset(data_poct, IDULast == 2 & Colour==12)))
    poct_idu_3 <- c(nrow(subset(data_poct, IDULast == 3)),
                   nrow(subset(data_poct, IDULast == 3 & Colour==12)))
    poct_idu_9 <- c(nrow(subset(data_poct, IDULast == 9 | is.na(IDULast))),
                   nrow(subset(data_poct, {IDULast == 9 | is.na(IDULast)} & Colour==12)))
    
    poct_idu <<- rbind(poct_idu_1,poct_idu_2,poct_idu_3,poct_idu_9)
    rm(poct_idu_1,poct_idu_2,poct_idu_3,poct_idu_9)
    
    ## Risk====
    poct_risk_1 <- c(
      nrow(subset(data_poct, PartnersLast== "O" & Sex=="M")),
      nrow(subset(data_poct, PartnersLast== "O"& Sex=="F")),
      nrow(subset(data_poct, PartnersLast== "O")),
      nrow(subset(data_poct, PartnersLast== "O" & Sex=="M" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "O"& Sex=="F" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "O" & Colour == 12)))
    poct_risk_2 <- c(
      nrow(subset(data_poct, PartnersLast== "B" & Sex=="M")),
      nrow(subset(data_poct, PartnersLast== "B"& Sex=="F")),
      nrow(subset(data_poct, PartnersLast== "B")),
      nrow(subset(data_poct, PartnersLast== "B" & Sex=="M" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "B"& Sex=="F" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "B" & Colour == 12)))
    poct_risk_3 <- c(
      nrow(subset(data_poct, PartnersLast== "S" & Sex=="M")),
      nrow(subset(data_poct, PartnersLast== "S"& Sex=="F")),
      nrow(subset(data_poct, PartnersLast== "S")),
      nrow(subset(data_poct, PartnersLast== "S" & Sex=="M" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "S"& Sex=="F" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "S" & Colour == 12)))
    poct_risk_4 <- c(
      nrow(subset(data_poct, PartnersLast== "N" & Sex=="M")),
      nrow(subset(data_poct, PartnersLast== "N"& Sex=="F")),
      nrow(subset(data_poct, PartnersLast== "N")),
      nrow(subset(data_poct, PartnersLast== "N" & Sex=="M" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "N"& Sex=="F" & Colour == 12)),
      nrow(subset(data_poct, PartnersLast== "N" & Colour == 12)))
    poct_risk_5 <- c(
      nrow(subset(data_poct, is.na(Partners) & Sex=="M")),
      nrow(subset(data_poct, is.na(Partners) & Sex=="F")),
      nrow(subset(data_poct, is.na(Partners))),
      nrow(subset(data_poct, is.na(Partners) & Sex=="M" & Colour == 12)),
      nrow(subset(data_poct, is.na(Partners) & Sex=="F" & Colour == 12)),
      nrow(subset(data_poct, is.na(Partners) & Colour == 12)))
    
    poct_risk <<- rbind(poct_risk_1,poct_risk_2,poct_risk_3,poct_risk_4,poct_risk_5)
    rm(poct_risk_1,poct_risk_2,poct_risk_3,poct_risk_4,poct_risk_5)
    
    ## SW====
    poct_sw_1 <- c(nrow(subset(data_poct, SWLast == 1)),
                     nrow(subset(data_poct, SWLast == 1 & Colour==12))                 )
    poct_sw_2 <- c(nrow(subset(data_poct, SWLast == 2)),
                     nrow(subset(data_poct, SWLast == 2 & Colour==12)))
    poct_sw_3 <- c(nrow(subset(data_poct, SWLast == 3)),
                     nrow(subset(data_poct, SWLast == 3 & Colour==12)))
    poct_sw_9 <- c(nrow(subset(data_poct, SWLast == 9 | is.na(SWLast))),
                     nrow(subset(data_poct, {SWLast == 9 | is.na(SWLast)} & Colour==12)))
    
    poct_sw <<- rbind(poct_sw_1,poct_sw_2,poct_sw_3,poct_sw_9)
    rm(poct_sw_1,poct_sw_2,poct_sw_3,poct_sw_9)
    
    ## Condom Use====
    poct_condom_use_1 <- c(
      nrow(subset(data_poct_male, CondomRegularMaleAnal==1)),
      nrow(subset(data_poct_male,  CondomCasualMaleAnal==1)),
      nrow(subset(data_poct_male, CondomRegularFemale==1)),
      nrow(subset(data_poct_male,  CondomCasualFemale==1)))
  
    poct_condom_use_2 <- c(
      nrow(subset(data_poct_male, CondomRegularMaleAnal==2)),
      nrow(subset(data_poct_male,  CondomCasualMaleAnal==2)),
      nrow(subset(data_poct_male, CondomRegularFemale==2)),
      nrow(subset(data_poct_male,  CondomCasualFemale==2)))
  
    poct_condom_use_3 <- c(
      nrow(subset(data_poct_male, CondomRegularMaleAnal==3)),
      nrow(subset(data_poct_male,  CondomCasualMaleAnal==3)),
      nrow(subset(data_poct_male, CondomRegularFemale==3)),
      nrow(subset(data_poct_male,  CondomCasualFemale==3)))
  
    poct_condom_use_4 <- c(
      nrow(subset(data_poct_male, CondomRegularMaleAnal==4)),
      nrow(subset(data_poct_male,  CondomCasualMaleAnal==4)),
      nrow(subset(data_poct_male, CondomRegularFemale==4)),
      nrow(subset(data_poct_male,  CondomCasualFemale==4)))
  
    poct_condom_use_5 <- c(
      nrow(subset(data_poct_male, CondomRegularMaleAnal==5)),
      nrow(subset(data_poct_male,  CondomCasualMaleAnal==5)),
      nrow(subset(data_poct_male, CondomRegularFemale==5)),
      nrow(subset(data_poct_male,  CondomCasualFemale==5)))
  
  
    poct_condom_use <<- rbind(poct_condom_use_1,poct_condom_use_2,poct_condom_use_3,poct_condom_use_4,poct_condom_use_5)
    rm(poct_condom_use_1,poct_condom_use_2,poct_condom_use_3,poct_condom_use_4,poct_condom_use_5)
    
    ## Previous Diagnoses====
    poct_syph <- c(nrow(subset(data_poct, SyphilisDiag==1)),
                   nrow(subset(data_poct, SyphilisDiag==1 & Colour==12)))
    poct_hcv <- c(nrow(subset(data_poct,  HepCDiag==1)),
                  nrow(subset(data_poct, HepCDiag==1 & Colour==12)))
    poct_diag <<- rbind(poct_hcv,poct_syph)
    rm(poct_hcv,poct_syph)
    
    ## Partners====
   
    poct_partners_1 <- c(
      nrow(subset(data_poct_msm, PartnersMaleLast3 %in% 1:5)),
      nrow(subset(data_poct_msm, PartnersMaleLast3 %in% 1:5 & Colour==12)))
    poct_partners_2 <- c(
      nrow(subset(data_poct_msm, PartnersMaleLast3 > 5)),
      nrow(subset(data_poct_msm, PartnersMaleLast3 > 5 & Colour==12))) 
    
    poct_partners<<- rbind(poct_partners_1,poct_partners_2)
    rm(poct_partners_1,poct_partners_2)
    
    ## Testing History====
    
    poct_testing_history_total <<- nrow(subset(data_poct, HIVEverTested==1))
    
    poct_testing_history_last <<- c(
      nrow(subset(data_poct,HIVLastTested==1)),
      nrow(subset(data_poct,HIVLastTested==2)),
      nrow(subset(data_poct,HIVLastTested==3)),
      nrow(subset(data_poct,HIVLastTested==4)),
      nrow(subset(data_poct,HIVLastTested==5)),
      nrow(subset(data_poct,HIVLastTested==6)))
    poct_testing_history_type <<- c(
      nrow(subset(data_poct,HIVLastTestType==1)),
      nrow(subset(data_poct,HIVLastTestType==2)),
      nrow(subset(data_poct,HIVLastTestType==3)),
      nrow(subset(data_poct,HIVLastTestType==4)),
      nrow(subset(data_poct,HIVLastTestType==5)))
    poct_testing_history_where <<- c(
      nrow(subset(data_poct,HIVWhereTested==1)),
      nrow(subset(data_poct,HIVWhereTested==2)),
      nrow(subset(data_poct,HIVWhereTested==3)),
      nrow(subset(data_poct,HIVWhereTested==4)),
      nrow(subset(data_poct,HIVWhereTested==5)),
      nrow(subset(data_poct,HIVWhereTested==6)))
    }
