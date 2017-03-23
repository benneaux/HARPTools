library(RODBC)

service_query <- 
    "SELECT tblVisit.URNO, 
      tblVisit.VisitDate, 
      tblCodeServiceType.ServiceType, 
      tblCodeClinic.Clinic, 
      tblServiceActivities.MainService
    FROM tblVisit 
      INNER JOIN (tblServiceActivities 
        INNER JOIN tblCodeServiceType 
          ON tblServiceActivities.MainService = tblCodeServiceType.ServiceCode)
          ON (tblServiceActivities.VisitType = tblVisit.VisitType) 
          AND (tblServiceActivities.VisitDate = tblVisit.VisitDate) 
          AND (tblServiceActivities.URNO = tblVisit.URNO)
        INNER JOIN tblCodeClinic ON tblVisit.Clinic = tblCodeClinic.ClinicNumber
    GROUP BY tblVisit.URNO, 
             tblVisit.VisitDate,
             tblCodeServiceType.ServiceType, 
             tblCodeClinic.Clinic, 
             tblServiceActivities.MainService
    HAVING ((tblServiceActivities.MainService) In (51,52));"

conn <- odbcConnect("SHIPHNE")

service_import <- as.data.frame(sqlQuery(conn,service_query))


client_query <- 
  "SELECT  tblClient.URNO,
           tblClient.MRN, 
           tblClient.Sex, 
           tblClient.DOB, 
           tblClient.Indigenous,
           tblClient.Countryofbirth
  
  FROM      tblClient
  
  GROUP BY  tblClient.URNO,
            tblClient.MRN, 
            tblClient.Sex, 
            tblClient.DOB, 
            tblClient.Indigenous,
            tblClient.Countryofbirth;"

client_import <- as.data.frame(sqlQuery(conn,client_query))

countrycodes <- sqlFetch(conn,"tblCodeCountries")

conn <- odbcConnect("VHS")

VHS_1st_screen_query <- 
  "SELECT VH_PATIENT.AUID,
         VH_APPOINTMENT.APPOINTMENT_DTTM, 
         VH_APPOINTMENT.APPOINTMENT_TYPE, 
         VH_APPOINTMENT.ATTENDED_YN

  FROM (VH_PATIENT 
        INNER JOIN VH_EPISODE 
          ON VH_PATIENT.PATIENT_ID = VH_EPISODE.PATIENT_ID) 
        INNER JOIN VH_APPOINTMENT 
          ON VH_EPISODE.EPISODE_ID = VH_APPOINTMENT.EPISODE_ID

  WHERE (((VH_APPOINTMENT.APPOINTMENT_TYPE)='S') AND ((VH_APPOINTMENT.ATTENDED_YN)='Y'));"

VHS_1st_screen_import <- as.data.frame(sqlQuery(conn,VHS_1st_screen_query))

odbcCloseAll()