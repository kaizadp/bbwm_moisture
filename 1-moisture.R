library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(drake)

# import all files 
moisture = sapply(list.files(path = "moisture/",pattern = "*.csv", full.names = TRUE),
                  read_csv, simplify = FALSE) %>% bind_rows()

# process and clean the data
moisture_plan = drake_plan(
  
  moisture2 = 
  moisture %>% 
  dplyr::mutate(
# this parses only those formats with time in HH:MM (24-hour) format. AM/PM formats are NA
    dt1 = parse_datetime(Datetime, "%m%.%d%.%Y%T"),
# this tries to parse everythng, but only does II:MM:SS AM/PM correctly. the HH:MM (24-hour) is parsed wrong
    dt2 = mdy_hms(Datetime),
    dt3 = mdy_hm(Datetime),
# so for now, parse both ways, and then select only the relevant cells to make a new combined column.
    DATETIME_combined = case_when(!is.na(dt1)~dt1,
                                  !is.na(dt2)~dt2,
                                  !is.na(dt3)~dt3)) %>% 
# recode depth values  
  dplyr::mutate(DEPTH = case_when(Depth=="10cm"~"10cm",
                                  Depth=="Depth1"~"10cm",
                                  Depth=="Depth1_10cm?"~"10cm",
                                  Depth=="25cm"~"25cm",
                                  Depth=="Depth2"~"25cm",
                                  Depth=="Probe# 635291"~"10cm",
                                  Depth=="Probe# 770804"~"25cm",
                                  Depth=="Unknown_10cm?"~"10cm",
                                  is.na(Depth)~"10cm",
                                  Depth=="vertical"~"vertical"),
                YEAR = year(DATETIME_combined),
                MONTH = month(DATETIME_combined),
                DAY = day(DATETIME_combined)) %>% 
# remove flagged values
  filter(is.na(Remove)) %>%
# remove empty rows  
  filter(!is.na(Moisture_m3_m3)) %>%
# remove values that don't make sense
  filter(!Moisture_m3_m3<=0.05) %>% 
  filter(!Moisture_m3_m3>0.4),
  

# WBSW looks weird. do QC
ggplot(moisture2[moisture2$Compartment=="WBSW" &
                   moisture2$YEAR>2012,], aes(x = DATETIME_combined, y = Moisture_m3_m3, color = DEPTH))+
  geom_point(),
  
ggplot(moisture2, aes(x = DATETIME_combined, y = Moisture_m3_m3, color = DEPTH))+
  geom_point()+
  facet_grid(Compartment~.),


# flagging suspect data for WBSW (early 2013)
moisture3=
  moisture2 %>% 
  dplyr::mutate(Remove = case_when((Compartment=="WBSW"&
                                     YEAR==2013&
                                     MONTH==1&
                                     DAY<13)~"Remove")) %>% 
  filter(is.na(Remove)),

# now, we have the corrected data. clean up
moisture_3hour = 
  moisture3 %>% 
  dplyr::mutate(Forest = case_when(grepl("HW",Compartment)~"HW",
                                   grepl("SW",Compartment)~"SW"),
                Watershed = case_when(grepl("EB",Compartment)~"EB",
                                      grepl("WB",Compartment)~"WB")) %>% 
  dplyr::select(Compartment, Watershed, Forest,
                DATETIME_combined, YEAR, MONTH, DAY, DEPTH,
                Moisture_m3_m3) %>% 
  rename(DATETIME = DATETIME_combined),

moisture_daily = 
  moisture_3hour %>% 
  group_by(Compartment, Watershed, Forest, YEAR, MONTH, DAY, DEPTH) %>% 
  dplyr::summarise(Moisture_m3_m3 = round(mean(Moisture_m3_m3, na.rm = TRUE),4)) %>%   
  dplyr::mutate(DATE = ymd(paste(YEAR,MONTH,DAY, sep="/"))),

### OUTPUT
write.csv(moisture_3hour,"processed/bbwm_moisture_3hour.csv", row.names = FALSE),
write.csv(moisture_daily,"processed/bbwm_moisture_daily.csv", row.names = FALSE)

)


make(moisture_plan)

moisture3=readd(moisture3)
moisture4 = moisture3 %>% 
  filter(is.na(DATETIME_combined)) %>% 
  dplyr::mutate(dt = mdy_hm(Datetime))
