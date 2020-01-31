# 
# Purpose: To estimate metabolism at Dampierre in 1994
# Author: Jake Diamond
# Date: July 22, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(streamMetabolizer)


# Data load and clean -----------------------------------------------------
# Load discharge data
df_q <- read_tsv("Data/Discharge/K4180010.txt")

# Load DO data
df <- read_excel("Data/EDF/edf_1993_2000.xlsx",
                 sheet = 1,
                 col_names = c("code", "var", "datetime", "value",
                               "qc_code", "valid_code"),
                 skip = 1)

# Some data cleaning
df <- df %>%
  separate(var, c("var", "site"), " de la ") %>%
  mutate(site = str_sub(site, end = -7),
         var = recode(var,
                      `Température de l'eau horaire` = "temp.water",
                      `pH horaire` = "pH",
                      `Oxygène dissous horaire` = "DO.obs",
                      `Conductivité horaire` = "SC"),
         datetime = dmy_hms(datetime, tz = "UTC")
         )

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Site name cleaning and select only dampierre 1994
dam <- df %>%
  mutate(site = case_when(grepl("Vienne", site) ~ "vienne",
                          grepl("Belleville", site) ~ "belleville",
                          grepl("Chinon", site) ~ "chinon",
                          grepl("Dampierre", site) ~ "dampierre",
                          grepl("Saint", site) ~ "saint_laurent"),
         year = year(datetime),
         date = date(datetime)) %>%
  filter(site == "dampierre",
         var %in% c("DO.obs", "temp.water"),
         year == 1996) %>%
  select(var, value, datetime) %>%
  spread(var, value)

# Combine with discharge data
dam <- df_q %>%
  filter(Annee == 1996) %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  rename(discharge = Qm3s) %>%
  select(discharge, date) %>%
  right_join(dam %>%
               mutate(date = date(datetime)))
  
  
  