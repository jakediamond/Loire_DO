# 
# Purpose: To compare discharge among headwater sites
# Author: Jake Diamond
# Date: November 4, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)

# List all files
data_path <- "Data/Headwaters_DO/Discharge/QTFIX"
files <- dir(data_path, pattern = "*.txt")

# Read in all discharge data
df <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~read_delim(file.path(data_path, .),
                                         delim = ";", skip = 41))
         ) %>%
  unnest()

# Data cleaning
df <- df %>%
  rename_all(.funs = list(str_trim))
df <- df %>%
  mutate(Qls = as.numeric(Qls),
         Qmmj = as.numeric(Qmmj),
         date = ymd_hm(df$Date),
         site = word(filename, 1, sep = "_"))

# Data spread to wide format
df_w <- df %>%
  select(date, site, Qls) %>%
  pivot_wider(names_from = site,
              values_from = Qls,
              values_fill = list(Qls = NA))
  

df_w2 <- select(df_w, -date)

df_w3 <- df_w2 %>%
  select(1,3) %>%
  drop_na()

plot(df_w3)

plot(df_w2[,1], df_w2[,2])


df_d <- df %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date) %>%
  summarize(Q = mean(Qmmj, na.rm = TRUE))

df_d_w <- df_d %>%
  pivot_wider(names_from = site,
              values_from = Q,
              values_fill = list(Q = NA))


df_w3 <- df_d_w %>%
  select(2,12) %>%
  drop_na()

plot(df_w3)
summary(lm(df_w3))
