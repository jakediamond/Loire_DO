# 
# Purpose: To estimate hourly temperature at Dampierre 1993-2018
# Author: Jake Diamond
# Date: August 5, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(chillR)

# Load daily data and clean
df_day <- read.csv2("Data/Meteo/air_temp_daily_1976_2019.csv",
                   header = TRUE) %>%
  select(date = DATE,
         Tmin = TN,
         Tmax = TX) %>%
  mutate(date = ymd(date),
         Year = year(date),
         Month = month(date),
         Day = day(date)) %>%
  na.omit()
  
# Load hourly data and clean
df_h <- read.csv2("Data/Meteo/air_temp_hourly_2018_2019.csv",
                    header = TRUE) %>%
  select(datetime = DATE,
         temp = `T`) %>%
  mutate(datetime = ymd_h(datetime))

# Estimate hourly temp data with chillR package
df_conv <- stack_hourly_temps(df_day, latitude = 47.7)$hourtemps

# Compare quickly
test <- df_h %>%
  left_join(df_conv %>%
              mutate(datetime = as.POSIXct(df_conv$date + 
                                             hours(df_conv$Hour))))
# Good enough
plot(test$temp, test$Temp)

# Save hourly temperature data
df_conv %>%
  mutate(datetime = as.POSIXct(df_conv$date + 
                                 hours(df_conv$Hour))) %>%
  filter(datetime < ymd_hms("2018-10-17 12:00:00")) %>%
  select(datetime, temp = Temp) %>%
  bind_rows(df_h) %>%
  na.omit() %>%
  saveRDS(file = "Data/Meteo/air_temp_hourly_1976_2019")
