library(dplyr)
library(lubridate)
reportdata <- data %>%
  tbl_df() %>%
  distinct() %>%
  mutate(PartnersLast = toupper(PartnersLast)) %>%
  mutate(Sex = toupper(Sex)) %>%
  mutate(SW = case_when(is.na(.$SWLast) ~ 0,
                        .$SWLast < 3 ~ 1,
                        .$SWLast >= 3 ~ 0)) %>%
  select(-SWLast) %>%
  mutate(MSM = case_when(is.na(.$PartnersLast) ~ 0,
                         (.$Sex == "M" & .$PartnersLast == "S") ~ 1,
                         (.$Sex == "M" & .$PartnersLast == "B") ~ 1,
                         .$Sex == "F" ~ 0,
                         .$PartnersLast == "O" | .$PartnersLast == "N" ~ 0)) %>%
  mutate(Test = case_when(.$LabGroupCode == 12 ~ "HIV Lab",
                          .$LabGroupCode == 96 ~ "HIV POCT")) %>%
  select(-LabGroupCode) %>%
  mutate(Indigenous = case_when(is.na(.$Indigenous) ~ 0,
                                 .$Indigenous < 4 ~ 1,
                                 .$Indigenous >= 4 ~ 0)) %>%
  mutate(PartnersLast = case_when(is.na(.$PartnersLast) ~ "U",
                                  !is.na(.$PartnersLast) ~ .$PartnersLast)) %>%
  mutate(Year = year(.$VisitDate)) %>%
  mutate(Quarter = case_when(quarter(.$VisitDate) == 1 ~ "Q1",
                             quarter(.$VisitDate) == 2 ~ "Q2",
                             quarter(.$VisitDate) == 3 ~ "Q3",
                             quarter(.$VisitDate) == 4 ~ "Q4")) %>%
  mutate(Month = lubridate::month(.$VisitDate, label = TRUE))

#rm(data)