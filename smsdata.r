require(RODBC)
channel <- odbcConnect("SHIPHNE")


query <- c("SELECT SMS.URNO,
               SMS.SMSDate as SMS_Date,
               SMS.SMStext,
               SMS.SMSCode,
               SMS.SMSResult
               FROM dbo.tblSMS as SMS")

sms <- tibble::as_tibble(sqlQuery(channel,query))

sms <- mutate(
  sms, 
  SMS_Month = lubridate::month(SMS_Date),
  SMS_Year = lubridate::year(SMS_Date))
