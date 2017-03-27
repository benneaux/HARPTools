library(tidyverse)


countrycodes <- select(countrycodes, c(1:3))

VHS_1st_screen_data <- VHS_1st_screen_import %>%
  mutate(VisitDate = as.Date(APPOINTMENT_DTTM, tz = "Australia/Sydney")) %>%
  group_by(AUID) %>%
  filter(VisitDate == min(VisitDate)) %>%
  ungroup() %>%
  select(c(HNE_MRN = AUID, VisitDate))


service_data <- service_import %>%
  mutate(VisitDate = as.Date(VisitDate, tz = "Australia/Sydney")) %>%
  group_by(URNO,ServiceType,Clinic) %>%
  filter(VisitDate == min(VisitDate)) %>%
  filter(VisitDate >= startdate & VisitDate <= enddate) %>%
  ungroup()

client_data <- client_import %>%
  filter(URNO %in% service_data$URNO) %>%
  mutate(DOB = as.Date(DOB,tz =  "Australia/Sydney")) %>%
  mutate(MRN = as.integer(as.character(.$MRN))) %>%
  mutate_if(is.factor,toupper) %>%
  mutate(Indigenous = case_when(is.na(.$Indigenous) ~ "Unknown",
                                .$Indigenous == 9 ~ "Unknown",
                                .$Indigenous == 1 ~ "Aboriginal",
                                .$Indigenous == 2 ~ "Torres Strait Islander",
                                .$Indigenous == 3 ~ "Both Aboriginal and Torres Strait Islander",
                                .$Indigenous == 4 ~ "Neither")) %>%
  left_join(countrycodes,by = c("Countryofbirth"="CountryCode"))


comb_data <- inner_join(client_data,service_data, by ="URNO") %>%
  mutate(VisitAge = floor((as.integer(VisitDate) - as.integer(DOB))/365.25)) %>%
  select(-c(Countryofbirth, CDU)) %>%
  mutate(AgeRange = case_when(is.na(.$VisitAge) ~ "",
                              .$VisitAge < 15 ~ "Less that 15 years",
                              .$VisitAge > 64 ~ "65 years and over",
                              .$VisitAge < 20 ~ "15-19 years",
                              .$VisitAge < 25 ~ "20-24 years",
                              .$VisitAge < 30 ~ "25-29 years",
                              .$VisitAge < 35 ~ "30-34 years",
                              .$VisitAge < 40 ~ "35-39 years",
                              .$VisitAge < 45 ~ "40-44 years",
                              .$VisitAge < 50 ~ "45-49 years",
                              .$VisitAge < 55 ~ "50-54 years",
                              .$VisitAge < 60 ~ "55-59 years",
                              .$VisitAge < 65 ~ "60-64 years")) %>%
  mutate(VHS = case_when(.$MRN %in% VHS_1st_screen_data$HNE_MRN ~ 1)) %>%
  filter(is.na(VHS)) %>%
  select(-c(VHS))

screening_data <- comb_data %>%
  filter(MainService==51) %>%
  select(Country_of_Birth = Country, 
         Gender           = Sex, 
         Treatment_Site   = Clinic, 
         Age_Group        = AgeRange, 
         HNE_MRN          = MRN, 
         ATSI_Status      = Indigenous)

commencement_data <- comb_data %>%
  filter(MainService==52) %>%
  mutate(Trial = "") %>%
  select(ATSI_Status      = Indigenous,
         Gender           = Sex,
         Country_of_Birth = Country, 
         Treatment_Site   = Clinic, 
         Age_Group        = AgeRange,
         Trial,
         HNE_MRN          = MRN)

rm(list = setdiff(ls(),c("screening_data", "commencement_data")))
