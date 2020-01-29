library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)


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
