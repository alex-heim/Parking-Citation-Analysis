#load packages
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


###pull in raw data sheets and combine

P_2011 <- read_excel("~/Learning R/Parking-Citation-Analysis/Parking Citations in T2 07-01-2011 to 06-30-2012 as of 05-01-2016.xlsx")
P_2012 <- read_excel("~/Learning R/Parking-Citation-Analysis/Parking Citations in T2 07-01-2012 to 06-30-2013 as of 05-01-2016.xlsx")
P_2013 <- read_excel("~/Learning R/Parking-Citation-Analysis/Parking Citations in T2 07-01-2013 to 06-30-2014 as of 05-01-2016.xlsx")
P_2014 <- read_excel("~/Learning R/Parking-Citation-Analysis/Parking Citations in T2 07-01-2014 to 06-30-2015 as of 05-01-2016.xlsx")
P_2015 <- read_excel("~/Learning R/Parking-Citation-Analysis/Parking Citations in T2 07-01-2015 to 03-31-2016 as of 05-01-2016.xlsx")

cite_raw<-rbind(P_2011,P_2012,P_2013,P_2014,P_2015)

#save as csv
write.csv2(cite_raw,file = 'total citations.csv')

##feature cleanup

cite_raw<-cite_raw %>%
  mutate_each('factor',Entity_UID,Vehicle_Plate_Anon,Entity_Type,Billing_State_Abbrev,Billing_Postal_Code,
              Event_Street,Citation_Status,Meter_Violation,Meter_Number,Citation_Description_Code,CON_IS_WRITEOFF,
              CON_IS_WARNING,CON_IS_VOID,CON_IS_UNDER_APPEAL,CON_IS_SPECIAL_STATUS,CON_IS_SOURCE_MANUAL,CON_IS_PREENTERED,
              CON_IS_ON_ADMIN_HOLD,CON_IS_NO_CONTEST,Citation_Officer_UID,Vehicle_Make,Vehicle_Style)

cite_raw<-cite_raw %>%
  mutate(cite_year = year(Event_Date),
         cite_month = month(Event_Date),
         cite_day = day(Event_Date),
         cite_hour = hour(Event_Date),
         cite_wday = wday(Event_Date,label=TRUE),
         cite_wday_num = wday(Event_Date),
         cite_hourz = hour(Event_Date) + minute(Event_Date)/60) %>%
  mutate(weekend = ifelse(cite_wday_num %in% c(1,7),1,0),
         business_hours = ifelse(weekend == 0 & cite_hour >= 7 & cite_hour <=19,1,0))

cite_raw<-cite_raw %>%
  mutate(vehicle_age_at_cite = cite_year-Vehicle_Year)


##exploratory analysis

cite_count <- cite_raw %>%
  group_by(Citation_Description) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(count <150)

cite_raw$Citation_Description[ !(cite_raw$Citation_Description %in% cite_count$Citation_Description) ] <- 'Other'

car_count<-cite_raw %>%
  group_by(Vehicle_Make) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(count <171)
