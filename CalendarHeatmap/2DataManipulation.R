library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(plyr)
library(scales)
library(zoo)

df1 <- data %>%
  mutate(VisitDate = as.Date(VisitDate, tz = "Australia/Sydney")) %>%
  mutate(year = year(VisitDate)) %>%
  mutate(yearmonth = as.yearmon(VisitDate)) %>%
  mutate(yearmonthf = factor(yearmonth)) %>%
  mutate(week = week(VisitDate)) %>%
  mutate(weekdayf = wday(VisitDate)) %>%
  mutate(monthf = month(VisitDate, label = TRUE)) %>%
  filter(!(weekdayf == 1 | weekdayf == 7))

df2 <- data %>%
  mutate(VisitDate = as.Date(VisitDate, tz = "Australia/Sydney")) %>%
  group_by(VisitDate) %>%
  tally() %>%
  select(c(VisitDate, datetotal = n))

df3 <- data %>%
  mutate(VisitDate = as.Date(VisitDate, tz = "Australia/Sydney")) %>%
  mutate(year = year(VisitDate)) %>%
  group_by(year) %>%
  tally() %>%
  select(c(year,yeartotal = n))

df4 <- data %>%
  mutate(VisitDate = as.Date(VisitDate, tz = "Australia/Sydney")) %>%
  mutate(yearmonth = as.yearmon(VisitDate)) %>%
  group_by(yearmonth) %>%
  tally() %>%
  select(c(yearmonth, monthtotal = n))

df <- inner_join(df1,df2,by = "VisitDate")
df <- inner_join(df,df3, by = "year")
df <- inner_join(df,df4, by = "yearmonth")
  
df <- ddply(df,.(yearmonthf), transform, monthweek = 1+week-min(week))  # compute week number of month
df5 <- df %>%
  group_by(yearmonthf,monthweek) %>%
  tally() %>%
  select(c(yearmonthf,monthweek, monthweektotal = n))


df <- inner_join(df,df5,by = c("yearmonthf","monthweek"))
df6 <- df %>%
  select(yearmonthf,monthweek,weekdayf) %>%
  distinct() %>%
  group_by(yearmonthf,monthweek) %>%
  tally() %>%
  select(c(yearmonthf,monthweek, monthweekdays = n))

df <- inner_join(df,df6,by = c("yearmonthf","monthweek"))

df <- df %>% 
  mutate(adjmonthweektotal = monthweektotal/monthweekdays)

df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf","datetotal","monthweektotal","monthweekdays","adjmonthweektotal","monthtotal","yeartotal")]
df <- distinct(df)
rm(list = setdiff(ls(),"df"))

