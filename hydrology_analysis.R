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
<<<<<<< HEAD
# # Generate daily time series
=======
# Generate daily time series
>>>>>>> b932b3b7e82798c70c22dee711ea77fd6bc70bba
dat_seq <- data.frame(Date = seq(ymd("1993-01-01"), 
                                 ymd('2018-12-31'), 
                                 by = "days"))
# Load discharge data, but this is missing 1994-1995
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  select(Q = Qm3s, Date = date)

# Load 1994 and 1995 data, but need to make average daily to match
df_q <- read_xls("Data/Moatar_thesis/DAM95AMC.XLS",
                 sheet = 1) %>%
  bind_rows(read_xls("Data/Moatar_thesis/DAM94AMC.XLS",
                     sheet = 4)) %>%
  select(datetime = DATE, Q = DEB) %>%
  mutate(Date = date(datetime)) %>%
  group_by(Date) %>%
  summarize(Q = mean(Q, na.rm = TRUE)) %>%
  # drop_na() %>%
  bind_rows(df_q) %>%
  arrange(Date) %>%
  right_join(dat_seq) %>%
  filter(between(Date, ymd("1993-01-01"), ymd("2018-12-31"))
  ) %>%
  distinct() %>%
  mutate(Q = ifelse(Q < 0, NA, Q),
         Date = as.POSIXct(Date))
<<<<<<< HEAD

=======
# saveRDS(df_q, "Data/Discharge/dampierre_discharge")
>>>>>>> b932b3b7e82798c70c22dee711ea77fd6bc70bba
# Hydrostatistics
df_q_stat <- df_q %>%
  group_by(year(Date)) %>%
  nest() %>%
  mutate(ls = map(data, low.spells, threshold = 200)) %>%
  unnest(ls)

df_q_stat <- df_q %>%
  mutate(threshold = ifelse(Q < 200, 1, 0)) %>%
  group_by(year(Date)) %>%
  summarize(run = rle(df_q_stat$threshold))
<<<<<<< HEAD
=======


>>>>>>> b932b3b7e82798c70c22dee711ea77fd6bc70bba
