###############################################################################
#
# Quarterly Figures
#
###############################################################################

clientsquarter <- reportdata %>%
  tbl_df() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Test,Month)) %>%
  distinct() %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsquarter <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsMSMquarter <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  filter(MSM==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW, Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsATSIquarter <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  filter(Indigenous==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW,Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

QuarterlyData <- tbl_df(clientsquarter) %>%
  left_join(HIVTestsquarter, by = c("Year", "Quarter")) %>%
  left_join(HIVTestsMSMquarter, by = c("Year", "Quarter")) %>%
  left_join(HIVTestsATSIquarter, by = c("Year", "Quarter")) %>%
  plyr::rename(c("n.x" = "Clients", 
                 "n.y" = "HIVTests", 
                 "n.x.x" = "MSMTests",
                 "n.y.y" = "ATSITests"))

  QuarterlyData[is.na(QuarterlyData)] <- as.integer(0)
  QuarterlyData <- mutate(QuarterlyData, 
                          ATSIPerc = scales::percent(
                            QuarterlyData$ATSITests/QuarterlyData$HIVTests))
###############################################################################
#
# Monthly Figures
#
###############################################################################

clientsmonth <- reportdata %>%
  tbl_df() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Test,Quarter)) %>%
  distinct() %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsmonth <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsMSMmonth <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  filter(MSM==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW, Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsATSImonth <- reportdata %>%
  tbl_df() %>%
  filter(Test == "HIV Lab" | Test == "HIV POCT") %>%
  distinct() %>%
  filter(Indigenous==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW,Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

MonthlyData <- tbl_df(clientsmonth) %>%
  left_join(HIVTestsmonth, by = c("Year", "Month")) %>%
  left_join(HIVTestsMSMmonth, by = c("Year", "Month")) %>%
  left_join(HIVTestsATSImonth, by = c("Year", "Month")) %>%
  plyr::rename(c("n.x" = "Clients", 
                 "n.y" = "HIVTests", 
                 "n.x.x" = "MSMTests",
                 "n.y.y" = "ATSITests"))

MonthlyData[is.na(MonthlyData)] <- as.integer(0)
MonthlyData <- mutate(MonthlyData, 
                        ATSIPerc = scales::percent(
                          MonthlyData$ATSITests/MonthlyData$HIVTests))


