# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: August 5, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

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
  filter(between(date, ymd("2008-01-01"), ymd("2018-12-31")))

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)

# DO data load and clean --------------------------------------------------
# Load DO data
df <- readRDS("Data/all_DO_cleaned") %>%
  left_join(readRDS("Data/all_DO_data") %>%
              select(var, site, datetime, value) %>%
              filter(var == "T") %>%
              mutate(var = recode(var,
                                  `T` = "temp.water")) %>%
              spread(var, value))

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Clean data a bit
df <- df %>%
  filter(year > 2007) %>%
  mutate(time_frame = ifelse(year < 2013,
                             1,
                             2)) %>%
  select(DO_use, temp.water, site, datetime, time_frame) %>%
  rename(DO.obs = DO_use)

# Prepare data for stream Metabolizer -------------------------------------
# Calculate DO.sat, streamMetabolizer calculation
df$DO.sat <- calc_DO_sat(temp.water = df$temp.water,
                          pressure.air = calc_air_pressure(temp.air = u(15,
                                                                        "degC"),
                                                           elevation = u(118,
                                                                         "m")
                                                           )
                          )

# Florentina's DO.sat calculation
df$DO.sat <- ifelse(df$temp.water == 0,
                     0,
                     14.652 - 0.41022 * df$temp.water + 0.007991 * 
                       df$temp.water^2 - 0.000077774 * df$temp.water^3)

# Convert to solar time at Gien station
df$solar.time <- calc_solar_time(df$datetime, longitude = 2.5)

# Get rid of datetime
df$datetime <- NULL

# Caclculate light
df$light <- calc_light(solar.time = df$solar.time,
                       latitude = 47.7,
                       longitude = 2.5)

# Calculate depth
depth <- df_q %>%
  mutate(depth = 0.134 * discharge.daily^0.4125)

# Combine depth with streamMetabolizer data
df <- depth %>%
  right_join(df %>%
               mutate(date = date(solar.time))) %>%
  select(-date, -discharge.daily)

# Get rid of discharge unless pooling
# df$discharge <- NULL

# Make sure it's a dataframe and not a tbl_df
df <- as.data.frame(df)

# Create periods of 4 years and nest data
df_n <- df %>%
  filter(site == "dampierre") %>%
  select(-site) %>%
  group_by(time_frame) %>%
  nest() %>%
  left_join(df_q %>%
           mutate(time_frame = ifelse(year(date) < 2014,
                                      1,
                                      2)) %>%
             group_by(time_frame) %>%
             nest() %>%
             rename(data_q = data))
  

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
                     # , GPP_daily_mu = gpp_mean
                     # , K600_lnQ_nodes_centers = brks
                     # , K600_lnQ_nodes_meanlog = rep(log(k6), length(brks))
                     )
bayes_specs
# Fit the model with subsetted data ---------------------------------------
# Function apply streammetabolizer to nested data
met_fun <- function(data, data_q, bayes_specs = bayes_specs){
  metab(specs = bayes_specs, 
        data = as.data.frame(data), 
        data_daily = as.data.frame(data_q))
}
# Apply to nested data to keep it all tidy
# mm_all <- df_n %>%
#   mutate(mm = map2(data, data_q, ~met_fun(data = .x,
#                                              data_q = .y,
#                                              bayes_specs = bayes_specs))
         # )
mm_all2 <- df_n %>%
  # filter(time_frame == 2) %>%
  mutate(mm = map2(data, data_q, ~met_fun(data = .x,
                                          data_q = .y,
                                          bayes_specs = bayes_specs))
  )

# Inspect the model -------------------------------------------------------
mm
x <- get_fit(mm)$daily %>%
  select(ends_with("Rhat"))

mm <- readRDS("Data/Loire_DO/mm_2009_2011.rds")

# Look at priors/posteriors
plot_distribs(mm, "GPP_daily")
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
