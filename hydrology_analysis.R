# # 
# Purpose: To calculate hydrology statistics by year and classify years for Middle Loire
# Author: Jake Diamond
# Date: November 11, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidyverse)
library(tseries)
library(readxl)
library(hydrostats)

# Load Q data
# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab") %>%
  mutate(Q = ifelse(discharge.daily < 0, NA, discharge.daily),
         Date = as.POSIXct(date)) %>%
  select(Q, Date)

# saveRDS(df_q, "Data/Discharge/dampierre_discharge")

# Hydrostatistics
df_q_stat <- df_q %>%
  group_by(year(Date)) %>%
  nest() %>%
  mutate(ls = map(data, low.spells, threshold = 200)) %>%
  unnest(ls)
mean(df_q_stat$avg.low.spell.duration)
df_lowflows <- df_q_stat %>%
  select(year = `year(Date)`, lf_dur = max.low.duration)
saveRDS(df_lowflows, "Data/Discharge/low_flow_duration")
df_lowflows <- readRDS("Data/Discharge/low_flow_duration")
df_q_stat2 <- df_q %>%
  mutate(threshold = ifelse(Q < 200, 1, 0)) %>%
  group_by(year(Date)) %>%
  summarize(run = rle(df_q_stat$threshold))



df_met_l %>%
  mutate(year = year(date)) %>%
  group_by(year, key) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE)) %>%
  left_join(df_lowflows) %>%
  # left_join(df_q_stat %>%
  #             select(year = "year(Date)", ls = avg.low.spell.duration)) %>%
  mutate(rel = mean / lf_dur,
         relmed = median /lf_dur,
         pd = if_else(year<2013, 0, 1)) %>%
  group_by(pd, key) %>% 
  summarize(mean = mean(rel, na.rm = TRUE), 
            median = median(relmed, na.rm = TRUE))
