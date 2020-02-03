# 
# Purpose: To estimate hourly water temperature data from min max
# Author: Jake Diamond
# Date: November 11, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)
library(zoo)
# Load all hourly temperature data
df <- readRDS("Data/all_DO_data") %>%
              select(var, site, datetime, value) %>%
              filter(var == "T") %>%
              mutate(var = recode(var,
                                  `T` = "temp.water")) %>%
              spread(var, value)

df_dam <- filter(df, site == "dampierre") %>%
  mutate(time = row_number())

# Load daily data 
df_d <- read_xlsx("Data/Loire_DO/Temperature_Dam_Chi.xlsx")

# Create a curve from the daily min max data at chinon
# First get an hourly sequence
time <- tibble(datetime = seq(ymd_h("1993-01-01 00"), ymd_h("2010-12-30 23"), 
                              by = "hour")) %>%
  mutate(time = row_number(),
         date = date(datetime))

# Now estimate with sine curve
t_est <- df_d %>%
  select(date, starts_with("chinon")) %>%
  na.trim() %>%
  mutate(date = date(date)) %>%
  left_join(time) %>%
  group_by(date) %>%
  mutate(amp = 0.5 * (chinon_max - chinon_min),
         temp_est = amp * sin((2 * pi / 24) * (time - 6)) + chinon_avg
         )

# By year estimate relationship between chinon and dampierre
df_mods <- df_d %>%
  group_by(year) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(chinon_avg ~ dampierre_avg, .)),
         summary = map(mod, broom::tidy)) %>%
  unnest(summary) %>%
  select(-data, -mod) %>%
  filter(term == "dampierre_avg")

t_dam <- t_est %>%
  mutate(year = year(date)) %>%
  left_join(df_mods) %>%
  mutate(temp.water = temp_est * estimate) %>%
  filter(between(date, ymd("2001-01-01"), ymd("2007-12-31"))) %>%
  select(date, datetime, temp.water)

saveRDS(t_dam, "Data/Loire_DO/dampierre_temp_estimates")
