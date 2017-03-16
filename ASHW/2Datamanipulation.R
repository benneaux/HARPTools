
library(knitr)
library(dplyr)

reportdata <- data %>%
  mutate(Year = year(VisitDate)) %>%
  mutate(visityearq = quarter(VisitDate, with_year = TRUE)) %>%
  filter(Year > 2012) %>%
  mutate(quarter = quarter(VisitDate)) %>%
  group_by(Year,visityearq,quarter) %>%
  tally() %>%
  ungroup() %>%
  mutate(Quarter = case_when(.$quarter == "1" ~ "Q1",
                                 .$quarter == "2" ~ "Q2",
                                 .$quarter == "3" ~ "Q3",
                                 .$quarter == "4" ~ "Q4")) %>%
  mutate(QuarterName = case_when(.$quarter == "1" ~ "Jan-Mar",
                                 .$quarter == "2" ~ "Apr-Jun",
                                 .$quarter == "3" ~ "Jul-Sep",
                                 .$quarter == "4" ~ "Oct-Dec")) %>%
  rename(OOS = n)

tbldata <- reportdata %>%
  tbl_df() %>%
  arrange(Year,Quarter) %>%
  select(Year, QuarterName, OOS) %>%
  rename(Quarter = QuarterName)

maxdata <- reportdata %>%
  tbl_df() %>%
  slice(which.max(reportdata$OOS))

mindata <- reportdata %>%
  tbl_df() %>%
  slice(which.min(reportdata$OOS))
