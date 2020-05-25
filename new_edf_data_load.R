options(java.parameters = "-Xmx4g" )
library(XLConnect)
library(xlsx)
library(readxl)
library(writexl)
library(furrr)
library(lubridate)
library(tidyverse)

setwd("//ly-lhq-srv/jake.diamond/Loire_DO")

# Load new data from EDF
# Load data
df_new <- read_excel("Data/EDF/ID4983 Horaire - tous - 2000 à 2008 et 2018 à 2019.xlsx",
                 sheet = 1,
                 col_names = c("code", "var", "datetime", "value",
                               "qc_code", "valid_code"),
                 
                 skip = 1) %>%
  mutate(datetime = dmy_hms(datetime, tz = "UTC")) %>%
  bind_rows(read_excel("Data/EDF/ID4983 Horaire - tous - 2000 à 2008 et 2018 à 2019.xlsx",
                       sheet = 2,
                       col_names = c("code", "var", "datetime", "value",
                                     "qc_code", "valid_code"),
                       skip = 1) %>%
              mutate(datetime = dmy_hms(datetime, tz = "UTC"))) %>%
  bind_rows((read_excel("Data/EDF/ID4983 Horaire - tous - 2000 à 2008 et 2018 à 2019.xlsx",
                        sheet = 3,
                        col_names = c("code", "var", "datetime", "value",
                                      "qc_code", "valid_code"),
                        skip = 1)) %>%
              mutate(datetime = dmy_hms(datetime, tz = "UTC")))

# Some data cleaning
df_new2 <- df_new %>%
  mutate(var = str_replace_all(var, c(`Température de l'eau horaire` = "temp",
                                      " du " = " de la ",
                                      "de l'Ain" = "de la Ain"))) %>%
  separate(var, c("var", "site"), " de la ") %>%
  mutate(site = str_sub(site, end = -7),
         var = recode(var,
                      "temp" = "T",
                      `pH horaire` = "pH",
                      `Oxygène dissous horaire` = "DO",
                      `Conductivité horaire` = "SC")
  )

# Get data in wide format for excel and florentina to use

df_wide <- df_new2 %>%
  select(-code) %>%
  pivot_wider(names_from = var, values_from = value)

df_wide %>%
  group_split(site) -> list_of_dfs
list_of_dfs %>%
  purrr::map(~pull(.,site)) %>% # Pull out site variable
  purrr::map(~as.character(.)) %>% # Convert factor to character
  purrr::map(~unique(.)) %>%
  # purrr::map(~word(.,1, -1)) %>%
  purrr::map(~str_trunc(., 25)) %>%
  purrr::map(~str_replace(.," à ", " ")) %>%
  purrr::map(~str_replace(.,"ô", "o"))-> names(list_of_dfs) # Set this as names for list members
names(list_of_dfs)
# wb <- createWorkbook()
# sheetnames <- names(list_of_dfs) # or names(datas) if provided
# sheets <- lapply(sheetnames, createSheet, wb = wb)
# void <- future_map2(list_of_dfs, sheets, addDataFrame)
# saveWorkbook(wb, file = "Data/Loire_DO/new_EDF_data.xlsx")

list_of_dfs %>%
  writexl::write_xlsx(path = "C:/Users/jake.diamond/Desktop/new_EDF_data.xlsx")

write_excel_csv2(df_wide, path = "Data/Loire_DO/new_EDF_data.csv")

# # Site name cleaning
# df <- df_new2 %>%
#   mutate(site = case_when(grepl("Vienne", site) ~ "vienne",
#                           grepl("Belleville", site) ~ "belleville",
#                           grepl("Chinon", site) ~ "chinon",
#                           grepl("Dampierre", site) ~ "dampierre",
#                           grepl("Saint", site) ~ "saint_laurent"))
#                           
# Summarize the data, meta
library(skimr)
summ <- df_new2 %>%
  mutate(value = as.numeric(str_replace(value, ",", "."))) %>%
  dplyr::select(site, var, datetime, value) %>%
  # pivot_wider(names_from = var, values_from = value) %>%
  group_by(site, var) %>%
  skim()

summ2 <- summ %>%
  dplyr::select(site, var = skim_variable, missing = n_missing, 
               complete_rate, POSIXct.min, POSIXct.max) %>%
  mutate(total = missing / complete_rate) %>%
  pivot_wider(names_from = c(var), values_from = c(POSIXct.min, POSIXct.max))
# Write to file
write_excel_csv2(summ, "Data/summary.csv")
# saveRDS(df, file = "Data/all_DO_data")

# Daily average, magnitude, and min
df_day <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date, var) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            mag = max(value, na.rm = TRUE) - min) %>%
  ungroup()


# Load DO data
df_old <- readRDS("Data/all_DO_cleaned")