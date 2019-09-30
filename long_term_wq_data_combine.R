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
library(ggpubr)
library(tabulizer)
library(readxl)

# Load data
# First get data path and names of files
data_path <- "Data/Long_term"
files <- dir(data_path, pattern = "*.txt")

# Then load all data into one dataframe
df <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                           ~ read_tsv(file.path(data_path, .)))
         ) %>%
  unnest()

# Some data cleaning, make filename = site and correct date
df <- df %>%
  mutate(filename = str_sub(df$filename, end = -5),
         Date = ymd(Date)) %>%
  rename(site_no = filename,
         date = Date)

# Save data
saveRDS(df, "Data/Headwaters_DO/all_longterm_wq_data")

# Determination of missing sites ------------------------------------------
# Load all site names
# First get data path and names of files
data_path_all <- "Data/Long_term_all"
sites_names_all <- dir(data_path_all, pattern = "*.txt")

# Then load all site names into one dataframe
sites_names_all <- tibble(site = sites_names_all) %>%
  mutate(site = str_sub(site, end = -5))


# Load station metadata from pdf
meta <- "References/Loire_meta_pdf.pdf"
m <- extract_areas(meta, pages = c(148:152),
                   encoding = "UTF-8",
                   method = "stream",
                   output = "data.frame")
sites_old <- as.data.frame(unlist(m)) %>%
  rename(site = `unlist(m)`) %>%
  filter(site != "national")

# Find old sites that are not in main file
sites_miss <- anti_join(sites_old, df, by = "site")
write_csv(sites_miss, path = "Data/missing_sites.csv")
sites_miss_all <- anti_join(sites_old, sites_names_all, by = "site")
write_csv(sites_miss_all, path = "Data/missing_sites_all.csv")
sites_miss_neither <- anti_join(sites_miss, sites_miss_all)
write_csv(sites_miss_neither, path = "Data/missing_sites_both_data_sources.csv")

# And the reverse
sites_extra <- anti_join(df, sites_old, by = "site") %>%
  select(site) %>%
  distinct()
write_csv(sites_extra, path = "Data/extra_sites.csv")

# Plot location of missing sites
loc <- read_excel("Data/RNB_stations_location/stations_RNB_Franceentiere.xlsx") %>%
  rename(site = CODE_STAT)
loc_miss <- loc %>%
  semi_join(sites_miss_neither)




