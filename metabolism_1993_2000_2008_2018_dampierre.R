# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: September 10, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

# Discharge data load and clean -----------------------------------------------------
# Generate daily time series
dat_seq <- data.frame(date = seq(ymd("1993-01-01"), 
                      ymd('2018-12-31'), 
                      by = "days"))

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
  # drop_na() %>%
  bind_rows(df_q) %>%
  arrange(date) %>%
  right_join(dat_seq) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2000-12-31")) |
           between(date, ymd("2008-01-01"), ymd("2018-12-31"))
         )

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)


# Load air temperature data and clean -------------------------------------
# Don't need this right now
# df_t <- readRDS("Data/Meteo/air_temp_hourly_1976_2019")
# # Force the correct time zone
# df_t$datetime <- force_tz(df_t$datetime, "Etc/GMT+1")

# DO data load and clean --------------------------------------------------
# Load DO data and join
df <- readRDS("Data/all_DO_cleaned") %>%
  left_join(readRDS("Data/all_DO_data") %>%
              select(var, site, datetime, value) %>%
              filter(var == "T") %>%
              mutate(var = recode(var,
                                  `T` = "temp.water")) %>%
              spread(var, value))

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Join with air temp to do barometric calculation
# Not doing this because only needed for DO.sat, which we have good estimate
# df <- left_join(df, df_t)

# Prepare data for stream Metabolizer -------------------------------------
# # Calculate DO.sat, streamMetabolizer calculation
# # It's nearly identical to Florentina's calculation, so use hers
# df$DO.sat <- calc_DO_sat(temp.water = df$temp.water,
#                           pressure.air = calc_air_pressure(temp.air = df$temp,
#                                                            elevation = 118
#                                                            )
#                           )

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

# Split data into four analysis periods to reduce memory needed
df <- df %>%
  mutate(time_frame = ifelse(between(year(solar.time), 
                                     1993, 
                                     1996),
                             1,
                             ifelse(between(year(solar.time), 
                                            1997, 
                                            2000),
                                    2,
                                    ifelse(between(year(solar.time), 
                                                   2008, 
                                                   2012),
                                           3,
                                           4)
                                    )
                             )
         ) %>%
  select(DO.obs = filtered, temp.water, site, 
         light, depth, DO.sat, solar.time
         , time_frame
         )

# Create periods of 4–6 years and nest data
df_n <- df %>%
  filter(site == "dampierre") %>%
  select(-site) %>%
  group_by(time_frame) %>%
  nest() %>%
  left_join(df_q %>%
              mutate(time_frame = ifelse(between(year(date), 
                                                 1993, 
                                                 1996),
                                         1,
                                         ifelse(between(year(date), 
                                                        1997, 
                                                        2000),
                                                2,
                                                ifelse(between(year(date), 
                                                               2008, 
                                                               2012),
                                                       3,
                                                       4)
                                                )
                                         )
                     ) %>%
             group_by(time_frame) %>%
             nest() %>%
             rename(data_q = data))
  
# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_mod <- mm_name(type = 'bayes', 
                      pool_K600 = 'binned', 
                      err_obs_iid = TRUE, 
                      err_proc_iid = TRUE)
bayes_mod

# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, data_q, bayes_name = bayes_mod){
  # Calculate the natural-log-space centers of the discharge bins
  # These are the bins for the time frame of 
  # Use the width method as in the help file with with = 0.8 log units
  brks <- calc_bins(vec = log(data_q$discharge.daily),
                    method = "width",
                    width = 0.8)$bounds
  
  # Estimate the mean ln(k600) value for the river from O'Connor and direct 
  # measurements with floating dome
  # Theis are the hyperprior mean for k600 in log space 
  k6 <- 0.19
  
  # Same for standard deviation
  k6_sd <- 0.22

  # Set the specifications
  bayes_specs <- specs(model_name = bayes_name,
                       burnin_steps = 1000,
                       saved_steps = 500
                       , K600_lnQ_nodes_centers = brks
                       , K600_lnQ_nodes_meanlog = rep(k6, 
                                                      length(brks))
                       , K600_lnQ_nodes_sdlog = rep(k6_sd, 
                                                    length(brks))
  )

  # Do the metabolism
  metab(specs = bayes_specs, 
        data = as.data.frame(data), 
        data_daily = as.data.frame(data_q))
}

# Run the metabolism model
# df_dam <- df %>%
#   filter(site == "dampierre",
#          solar.time > ymd_hms("1996-12-31 23:30:00")) %>%
#   select(-site) %>%
#   as.data.frame()
# df_q_dam <- df_q %>%
#   filter(date > ymd("1996-12-31")) %>%
#   as.data.frame()

# mm_all <- met_fun(df_dam, df_q_dam)
# plot_DO_preds(mm_all)
# get_params(mm_all)
# Run the metabolism model on nested data ---------------------------------
mm_all <- df_n %>%
  transmute(mm = map2(data, data_q, ~met_fun(data = .x,
                                             data_q = .y)
                      )
         )

saveRDS(mm_all, "Data/metab_constrainedK_1993_2018")
# Inspect the model -------------------------------------------------------
mm <- df_n %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met)

ggplot(data = mm, aes(x = date,
                      y = GPP)) + geom_point()

rh <- df_n %>%
  mutate(r = map(mm, get_fit)) %>%
  select(ends_with("Rhat"))
get_fit(mm_all)
mm <- readRDS("Data/Loire_DO/mm_2009_2011.rds")

kt <- df_n %>%
  mutate(mk = map(mm, get_params)) %>%
  unnest(mk)
plot(kt$K600.daily, kt$ER.daily)
plot(kt$date, kt$K600.daily)


# Look at priors/posteriors
plot_distribs(bayes_specs, "K600_daily_lnQ")
plot_distribs(mm_all, "K600_daily")
get_specs(mm_all)
# Daily metabolism predictions
predict_metab(mm)
plot_metab_preds(mm_all)
get_specs(mm)
get_params(mm)
plot_DO_preds(mm)
plot(get_params(mm_all)$K600.daily, predict_metab(mm_all)$ER)
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


s <- specs(bayes_name,
                     burnin_steps = 2000,
                     saved_steps = 500
                     # , K600_lnQ_nodes_centers = 8
                     , K600_daily_sigma = rep(k6h,
                                                    8)
                     # , K600_lnQ_nodes_sdlog = rep(k6_sdh, 
                     #                          8)
)
?plot_distribs
plot_distribs(s, "K600_daily_sigma")
