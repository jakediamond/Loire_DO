# 
# Purpose: To summarize long-term data for Loire River
# Author: Jake Diamond
# Date: June 11, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
# library(ggpubr)
# library(tabulizer)
# library(readxl)

# Load long term wq data
df <- readRDS("Data/Headwaters_DO/all_longterm_wq_data")

# Load metadata for site match with new sites
meta <- read_xlsx("Data/Headwaters_DO/site_longterm_match.xlsx")

# Match those data
df <- right_join(df, meta) %>%
  drop_na() %>%
  rename(DOC = COD,
         TKN = NKj,
         SiO2 = SIO,
         temp = TEMP_EAU,
         SC = COND,
         TP = PTO,
         DO = O2,
         year = Annee,
         month = Mois,
         day = Jour)

# Load our measured water chemistry
df_meas <- read_xlsx("Data/Headwaters_DO/water_chemistry/Water_chemistry_all_no_character.xlsx") %>%
  mutate(date = date(datetime),
         year = year(date),
         month = month(date),
         day = day(date)) %>%
  rename(DOC = `DOC-C`)

# And our measured field data
df_field <- read_xlsx("Data/Headwaters_DO/Field_data.xlsx") %>%
  select(site = Site,
         datetime = Datetime,
         temp = `T (°C)`,
         SC = `SC (us/cm)`,
         DO = `DO (mg/L)`,
         pH) %>%
  mutate(date = date(datetime),
         year = year(date),
         month = month(date),
         day = day(date))

# Bind all data
df <- bind_rows(df, df_field, df_meas)

# Save data
saveRDS(df, "Data/Headwaters_DO/all_wq_data")

# Quick look at time series
ggplot(df,
       aes(x = date,
           y = TP,
           color = month)) +
  geom_point() + facet_wrap(~site)
# Data analysis -----------------------------------------------------------
# List of variables to analyze
var <- 
# Some data summaries. Change all negatives to NA
df_sum <- df %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  group_by(site) %>%
  summarise_at(.vars = vars(temp:Mg),
               .funs = list(mean = mean,
                             sd = sd, 
                             max = max, 
                             min = min), 
                na.rm = TRUE)

saveRDS(df_sum, "Data/Headwaters_DO/all_wq_data_summary")

# df_sum_loc <- left_join(df_sum, loc, by = "site")
# write_csv2(df_sum_loc, path = "Data/loire_wq_loc.csv")
