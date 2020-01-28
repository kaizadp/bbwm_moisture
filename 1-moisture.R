library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)


moisture = sapply(list.files(path = "moisture/",pattern = "*.csv", full.names = TRUE),
                  read_csv, simplify = FALSE) %>% bind_rows()

moisture2 = 
  moisture %>% 
  dplyr::mutate(DATETIME = parse_date_time(Datetime, orders="mdy HMS"),
                datetime = mdy_hms(Datetime))

ggplot(moisture, aes(x = Datetime, y = Moisture_m3_m3))+
  geom_point()
                  