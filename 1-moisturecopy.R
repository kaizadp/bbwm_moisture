library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

<<<<<<< HEAD
# import all files 
moisture = sapply(list.files(path = "moisture/",pattern = "*.csv", full.names = TRUE),
                  read_csv, simplify = FALSE) %>% bind_rows()

# process and clean the data
moisture2 = 
  moisture %>% 
  dplyr::mutate(
# this parses only those formats with time in HH:MM (24-hour) format. AM/PM formats are NA
    Datetime2 = parse_datetime(Datetime, "%m%.%d%.%Y%T"),
# this tries to parse everythng, but only does II:MM:SS AM/PM correctly. the HH:MM (24-hour) is parsed wrong
    datetime = mdy_hms(Datetime),
# so for now, parse both ways, and then select only the relevant cells to make a new combined column.
    DATETIME_combined = case_when(!is.na(Datetime2)~Datetime2,
                                  is.na(Datetime2)~datetime)) %>% 
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
  filter(!Moisture_m3_m3>0.4)
  

# WBSW looks weird. do QC
ggplot(moisture2[moisture2$Compartment=="WBSW" &
                   moisture2$YEAR>2012,], aes(x = DATETIME_combined, y = Moisture_m3_m3, color = DEPTH))+
  geom_point()
  
ggplot(moisture2, aes(x = DATETIME_combined, y = Moisture_m3_m3, color = DEPTH))+
  geom_point()+
  facet_grid(Compartment~.)


# flagging suspect data for WBSW (early 2013)
moisture3=
  moisture2 %>% 
  dplyr::mutate(Remove = case_when((Compartment=="WBSW"&
                                     YEAR==2013&
                                     MONTH==1&
                                     DAY<13)~"Remove")) %>% 
  filter(is.na(Remove))

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
  rename(DATETIME = DATETIME_combined)

moisture_daily = 
  moisture_3hour %>% 
  group_by(Compartment, Watershed, Forest, YEAR, MONTH, DAY, DEPTH) %>% 
  dplyr::summarise(Moisture_m3_m3 = round(mean(Moisture_m3_m3, na.rm = TRUE),4)) %>%   
  dplyr::mutate(DATE = ymd(paste(YEAR,MONTH,DAY, sep="/")))


# plots
ggplot(moisture_daily, aes(x=DATE, y = Moisture_m3_m3, color=Watershed))+
  geom_point()+
  facet_grid(DEPTH~Forest)


=======

files <- list.files(path = "moisture/", pattern = "*.csv", full.names = TRUE)
message("Reading ", length(files), " files")
moisture <- sapply(files, read_csv, simplify = FALSE) %>% bind_rows()

# Timestamps here are a mess and in multiple formats. Parse using
# three different formats and pick the one that succeeds. I've
# checked (albeit not exhaustively) and don't see a case where
# dt1/dt2/dt3 parse validly but with different timestamps
moisture %>% 
  mutate(dt1 = mdy_hms(Datetime),
         dt2 = mdy_hm(Datetime),
         dt3 = ymd_hm(Datetime),
         DATETIME = case_when(!is.na(dt1) ~ dt1,
                              !is.na(dt2) ~ dt2,
                              !is.na(dt3) ~ dt3)) %>% 
  select(-dt1, -dt2, -dt3) ->
  moisture_parsed

p <- ggplot(moisture_parsed, aes(x = DATETIME, y = Moisture_m3_m3, color = Compartment))+
  geom_point()
print(p)
>>>>>>> dca6eecce07228ada09565e8a6a4335568f36b25
