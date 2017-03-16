
client_data <- readRDS(RDS_Name)
  
clientdata <- servicedata %>%
  filter(servicedata$URNO %in% client_data$MRN) %>%
  select(-c(Clinic, CentreCode, VisitDate, ResultCode, Result, LabGroup, LabGroupCode))

testdata <- servicedata %>%
  filter(servicedata$URNO %in% client_data$MRN) %>%
  select(-c(Sex, DOB, Indigenous, SW, IDU, Partners))

visitdata <- visitdata %>%
  filter(visitdata$URNO %in% client_data$MRN)

testing <- testdata %>%
  select(-c(ResultCode, Result, Test)) %>%
  distinct() %>%
  group_by(URNO, VisitDate, LabGroup) %>%
  tally() %>%
  spread(LabGroup, n, fill = 0)

results <- testdata %>%
  drop_na() %>%
  select(c(URNO,VisitDate,Test,Result,LabGroup))


