# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: August 20, 2019
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
# Load discharge data
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  rename(discharge.daily = Qm3s) %>%
  select(discharge.daily, date) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2000-12-31")))

# DO data load and clean --------------------------------------------------
# Load DO data
df <- readRDS("Data/all_DO_data")

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Some data cleaning
dam <- df %>%
  mutate(
         var = recode(var,
                      `T` = "temp.water",
                      `DO` = "DO.obs"),
         year = year(datetime),
         date = date(datetime),
         month = month(datetime)) %>%
  filter(site == "dampierre",
         var %in% c("DO.obs", "temp.water"),
        between(date, ymd("2008-01-01"), ymd("2018-12-31"))) %>%
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

# Nest data by year                            
# dam_nest <- dam %>%
#   mutate(year = year(solar.time)) %>%
#   group_by(year) %>%
#   nest()

# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_name <- mm_name(type = 'bayes', 
                      pool_K600 = 'binned', 
                      err_obs_iid = TRUE, 
                      err_proc_iid = TRUE)
bayes_name

# Calculate the natural-log-space centers of the discharge bins
# These are the bins 
# Use the width method as in the help file with with = 0.8 log units
# brks <- calc_bins(vec = log(df_q$discharge.daily),
#                   method = "width",
#                   width = 0.8)$bounds

# Estimate the k600 value for the river with Raymond et al. (2012), eq.7
# at baseflow, k600 = 4725*(VS)^0.86*Q^-0.14*D^0.66
# This is the hyperprior mean, 
# the prior distribution of the mean of K600 for all days
# k6 <- 4725 * (0.25 * 0.0001)^0.86 * 73^-0.14 * 1^0.66

# Set the specifications
bayes_specs <- specs(bayes_name,
                     burnin_steps = 1000,
                     saved_steps = 500
                     # ,K600_lnQ_nodes_centers = brks
                     # ,K600_lnQ_nodes_meanlog = rep(log(k6), 
                                                  # length(brks)
                                                  # )
                     )
bayes_specs


# Fit the model with subsetted data, for loop ---------------------------------------
for(i in 2008:2018){
  # get strings for subset of data
  dt_str_start <- paste0(i, "-01-01 01:09:58")
  dt_str_end <- paste0(i+2, "-12-31 00:09:58")
  d_str_start <- paste0(i, "-01-01")
  d_str_end <- paste0(i+2, "-12-31")
  # subset data
  dam_sub <- filter(dam,
                    between(solar.time,
                            ymd_hms(dt_str_start),
                            ymd_hms(dt_str_end)))
  df_q_sub <- filter(df_q,
                     between(date,
                             ymd(d_str_start),
                             ymd(d_str_end)))
  # Fit the model
  mm <- metab(bayes_specs, 
              data = dam_sub,
              data_daily = df_q_sub)

  # Save data
  saveRDS(mm,
          file = paste0("Data/Loire_DO/mm_", i, ".rds"))
  
  # Remove data
  rm(mm)
}
saveRDS(mm,
        file = "Data/Loire_DO/mm_2008_2011.rds")

