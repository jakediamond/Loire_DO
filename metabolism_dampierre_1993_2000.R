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

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

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
  dplyr::filter(site == "dampierre",
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

# Calculate depth
depth <- df_q %>%
  mutate(depth = 0.134 * discharge.daily^0.4125)

# Combine depth with streamMetabolizer data
dam <- depth %>%
  right_join(dam %>%
               mutate(date = date(solar.time))) %>%
  select(-date, -discharge.daily)

# Get rid of discharge unless pooling
# dam$discharge <- NULL

# Make sure it's a dataframe and not a tbl_df
dam <- as.data.frame(dam)
# dam_test <- filter(dam,
#                    between(solar.time,
#                            ymd_hms("1993-05-01 01:09:58"),
#                            ymd_hms("1993-10-01 00:09:58")))

# Nest data by year                            
# dam_nest <- dam %>%
#   mutate(year = year(solar.time)) %>%
#   group_by(year) %>%
#   nest()
  
# Configure the model -----------------------------------------------------
# First choose the time frame to model
yr_start <- 1993
yr_end <- 1993

# Choose if you want summer or not
summer <- "no"

# Get the data subset to this timefram
dt_str_start <- switch(summer,
                       no = paste0(yr_start, "-01-01 01:09:58"),
                       yes = paste0(yr_start, "-05-01 01:09:58")
                       )
dt_str_end <- switch(summer,
                     no = paste0(yr_end, "-12-31 00:09:58"),
                     yes = paste0(yr_end, "-09-30 00:09:58")
                     )
d_str_start <- switch(summer,
                      no = paste0(yr_start, "-01-01"),
                      yes = paste0(yr_start, "-05-01")
                      )
                      
d_str_end <- switch(summer,
                    no = paste0(yr_end, "-12-31"),
                    yes = paste0(yr_end, "-09-30")
                    )

# subset data
dam_sub <- filter(dam,
                  between(solar.time,
                          ymd_hms(dt_str_start),
                          ymd_hms(dt_str_end)))
df_q_sub <- filter(df_q,
                   between(date,
                           ymd(d_str_start),
                           ymd(d_str_end)))

# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_name <- mm_name(type = 'bayes', 
                      pool_K600 = 'binned', 
                      err_obs_iid = TRUE, 
                      err_proc_iid = TRUE)
bayes_name

# Calculate the natural-log-space centers of the discharge bins
# These are the bins for the time frame of 
# Use the width method as in the help file with with = 0.8 log units
brks <- calc_bins(vec = log(df_q_sub$discharge.daily),
                  method = "width",
                  width = 0.8)$bounds

# Estimate the k600 value for the river with Raymond et al. (2012), eq.7
# at baseflow, k600 = 4725*(VS)^0.86*Q^-0.14*D^0.66
# This is the hyperprior mean, 
# the prior distribution of the mean of K600 for all days
k6 <- 4725 * (0.25 * 0.0001)^0.86 * 73^-0.14 * 1^0.66

# Set the prior for mean GPP, guessing around 8, 
# because summer can be quite high
gpp_mean <- 8

# Set the specifications
bayes_specs <- specs(bayes_name,
                     burnin_steps = 2000,
                     saved_steps = 500
                     , GPP_daily_mu = gpp_mean
                     , K600_lnQ_nodes_centers = brks
                     , K600_lnQ_nodes_meanlog = rep(log(k6), length(brks))
                     )
bayes_specs


# Fit the model with subsetted data ---------------------------------------
mm <- metab(bayes_specs, 
            data = dam_sub,
            data_daily = df_q_sub)


# Inspect the model -------------------------------------------------------
mm
get_fit(mm)$daily %>%
  select(ends_with("Rhat"))

mm <- readRDS("Data/Loire_DO/mm_2009_2011.rds")

# Look at priors/posteriors
plot_distribs(bayes_specs, "K600_daily_lnQ")
plot_distribs(get_specs(mm), "GPP_daily")
get_specs(mm)
# Daily metabolism predictions
predict_metab(mm)
plot_metab_preds(mm)
get_specs(mm)
get_params(mm)
plot_DO_preds(mm)
plot(get_params(mm)$K600.daily, predict_metab(mm)$ER)
plot(get_params(mm)$date, get_params(mm)$K600.daily)
head(predict_DO(mm))
get
?plot_DO_preds()
saveRDS(mm, file = "Data/Loire_DO/mm_1993_1996")
mm <- readRDS("Data/Loire_DO/mm_1994.rds")

do_test <- lag(get_data(mm)$DO.obs) + 
  (predict_DO(mm)$DO.mod-lag(predict_DO(mm)$DO.mod))+
  summary(get_fit(mm)$inst$err_proc_iid_mean)
summary(get_fit(mm)$inst$err_obs_iid_mean)
plot(do_test)
