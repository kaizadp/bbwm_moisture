library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)


moisture = sapply(list.files(path = "moisture/",pattern = "*.csv", full.names = TRUE),
                  read_csv, simplify = FALSE) %>% bind_rows()

moisture2 = 
  moisture %>% 
  dplyr::mutate(
# this parses only those formats with time in HH:MM (24-hour) format. AM/PM formats are NA
    Datetime2 = parse_datetime(Datetime, "%m%.%d%.%Y%T"),
# this tries to parse everythng, but only does II:MM:SS AM/PM correctly. the HH:MM (24-hour) is parsed wrong
    datetime = mdy_hms(Datetime))
# so for now, parse both ways, and then select only the relevant cells to make a new combined column.

ggplot(moisture, aes(x = Datetime, y = Moisture_m3_m3))+
  geom_point()
                  