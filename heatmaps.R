# 
# Purpose: To look at spectral signatures of DO in EDF time series
# Author: Jake Diamond
# Date: November 8, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidyverse)
library(tseries)
library(readxl)

# Load DO data
df <- readRDS("Data/all_DO_cleaned")

# Load Q data
# # Generate daily time series
dat_seq <- data.frame(date = seq(ymd("1993-01-01"), 
                                 ymd('2018-12-31'), 
                                 by = "days"))
# Load discharge data, but this is missing 1994-1995
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  rename(discharge.daily = Qm3s) %>%
  select(discharge.daily, date)

# Load 1994 and 1995 data, but need to make average daily to match
df_q <- read_xls("Data/Moatar_thesis/DAM95AMC.XLS",
                 sheet = 1) %>%
  bind_rows(read_xls("Data/Moatar_thesis/DAM94AMC.XLS",
                     sheet = 4)) %>%
  select(datetime = DATE, discharge = DEB) %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(discharge.daily = mean(discharge, na.rm = TRUE)) %>%
  # drop_na() %>%
  bind_rows(df_q) %>%
  arrange(date) %>%
  right_join(dat_seq) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2018-12-31")))

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)
df_q$period <- ifelse(year(df_q$date) < 2001, 1, 2)

# Plot raw  min max data, but filtered to 7 days
df_sum <- df %>%
  filter(site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date, period) %>%
  summarize(min = min(DO_use),
            max = max(DO_use),
            amp = diff(range(DO_use))) %>%
  pivot_longer(-c(period, date), names_to = "type") %>%
  ungroup() %>%
  mutate(day = yday(date),
         month = month(date),
         year = year(date))


plot_func <- function(data, name) {
  ggplot(data = data, aes(x = year, y = day, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(name = name) +
    geom_hline(yintercept = 171) +
    geom_hline(yintercept = 265)
}

df_n <- df_sum %>% 
  group_by(type) %>% 
  nest() %>% 
  mutate(plots = map2(data, type, plot_func)) 

gridExtra::grid.arrange(grobs = df_n$plots)

