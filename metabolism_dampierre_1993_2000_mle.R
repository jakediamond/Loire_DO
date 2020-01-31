# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: August 5, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(dygraphs)

# Look at what data inputs are needed for bayes model
metab_inputs("mle", "data")

# Discharge data load and clean -----------------------------------------------------
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
  drop_na() %>%
  bind_rows(df_q) %>%
  arrange(date) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2000-12-31")))

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)

# DO data load and clean --------------------------------------------------
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

# Site name cleaning and select only dampierre
dam <- df %>%
  mutate(site = case_when(grepl("Vienne", site) ~ "vienne",
                          grepl("Belleville", site) ~ "belleville",
                          grepl("Chinon", site) ~ "chinon",
                          grepl("Dampierre", site) ~ "dampierre",
                          grepl("Saint", site) ~ "saint_laurent"),
         year = year(datetime),
         date = date(datetime),
         month = month(datetime)) %>%
  filter(site == "dampierre",
         var %in% c("DO.obs", "temp.water")) %>%
  select(var, value, datetime) %>%
  spread(var, value)

# Prepare data for stream Metabolizer -------------------------------------
# Calculate DO.sat, streamMetabolizer calculation
# dam$DO.sat <- calc_DO_sat(temp.water = dam$DO.sat,
#                           pressure.air = calc_air_pressure(temp.air = u(15,
#                                                                         "degC"),
#                                                            elevation = u(118,
#                                                                          "m")
#                                                            )
#                           )

# Florentina's DO.sat calculation
dam$DO.sat <- ifelse(dam$temp.water == 0,
                     0,
                     14.652 - 0.41022 * dam$temp.water + 0.007991 * 
                       dam$temp.water^2 - 0.000077774 * dam$temp.water^3)

# Convert to solar time at Gien station
dam$solar.time <- calc_solar_time(dam$datetime, longitude = 2.5)

# Get rid of datetime
dam$datetime <- NULL

# Caclculate light
dam$light <- calc_light(solar.time = dam$solar.time,
                       latitude = 47.7,
                       longitude = 2.5)

# Calculate depth (m) and depth (m/s), k600
dv <- df_q %>%
  mutate(depth = 0.134 * discharge.daily^0.4125,
         vel = 0.165 * discharge.daily^0.275)

# Combine depth with streamMetabolizer data
dam <- dv %>%
  right_join(dam %>%
               mutate(date = date(solar.time))) %>%
  select(-date, -discharge.daily,
         -vel)

# Estimate K600 with O'Connor and Dobbins
df_daily <- dv %>%
  transmute(date = date,
            K600.daily = 3.89 * (vel^0.5) / (depth^1.5)) %>%
  as.data.frame()

# Get rid of discharge unless pooling
dam$discharge <- NULL

# Make sure it's a dataframe and not a tbl_df
dam <- as.data.frame(dam)

# Configure the model -----------------------------------------------------
# First choose the time frame to model
yr_start <- 1993
yr_end <- 2000

# Choose if you want summer or not
summer <- "no"

# Get the data subset to this timefram
dt_str_start <- switch(summer,
                       no = paste0(yr_start, "-01-01 01:09:58"),
                       yes = paste0(yr_start, "-05-01 01:09:58")
                       )
dt_str_end <- switch(summer,
                     no = paste0(yr_end, "-01-01 00:09:58"),
                     yes = paste0(yr_end, "-09-30 00:09:58")
                     )
d_str_start <- switch(summer,
                      no = paste0(yr_start, "-01-01"),
                      yes = paste0(yr_start, "-05-01")
                      )
                      
d_str_end <- switch(summer,
                    no = paste0(yr_end, "-01-01"),
                    yes = paste0(yr_end, "-09-30")
                    )

# subset data
dam_sub <- filter(dam,
                  between(solar.time,
                          ymd_hms(dt_str_start),
                          ymd_hms(dt_str_end)))
# df_q_sub <- filter(df_q,
#                    between(date,
#                            ymd(d_str_start),
#                            ymd(d_str_end)))

# Choose a model structure
# We choose a maximum likelihood model
mle_name <- mm_name(type = 'mle')
mle_name

# Set the specifications
mle_specs <- specs(mle_name)
mle_specs


# Fit the model with subsetted data ---------------------------------------
mle <- metab(mle_specs, 
            data = dam_sub,
            data_daily = df_daily)


# Inspect the model -------------------------------------------------------
# Daily metabolism predictions
mle
predict_metab(mle)
plot_metab_preds(mle)
get_params(mle)
plot_DO_preds(mle)
plot(get_params(mle)$K600.daily, predict_metab(mle)$ER,
     xlim=c(0,10),
     ylim=c(-15,0))
plot(get_params(mle)$date, get_params(mle)$K600.daily,
    ylim=c(-5,10))

plot(df_q$discharge.daily, get_params(mle)$K600.daily)


df_DO <- predict_DO(mle)
bv_do <- df_DO %>%
  select(DO.obs, DO.mod) %>%
  zoo::zoo(order.by = df_DO$solar.time)
dygraph(bv_do, main = "Loire à Dampierre") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  # dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  dySeries("DO.mod", axis=('y')) %>%
  dySeries("DO.obs", axis=('y'), drawPoints = TRUE)




saveRDS(mm, file = "Data/Loire_DO/mm_1993_1996")
mm <- readRDS("Data/Loire_DO/mm_1994.rds")
