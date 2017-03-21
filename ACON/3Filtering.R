###############################################################################
#
# Quarterly Figures
#
###############################################################################

clientsquarter <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Test,Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsquarter <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsMSMquarter <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  filter(MSM==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW, Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

HIVTestsATSIquarter <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  filter(Indigenous==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW,Month)) %>%
  group_by(Year,Quarter) %>%
  tally() %>%
  ungroup()

###############################################################################
#
# Monthly Figures
#
###############################################################################

clientsmonth <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Test,Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsmonth <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,SW,MSM,Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsMSMmonth <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  filter(MSM==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW, Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()

HIVTestsATSImonth <- reportdata %>%
  tbl_df() %>%
  distinct() %>%
  filter(Indigenous==1) %>%
  select(-c(VisitDate,Sex,Indigenous,PartnersLast,MSM,SW,Quarter)) %>%
  group_by(Year,Month) %>%
  tally() %>%
  ungroup()



