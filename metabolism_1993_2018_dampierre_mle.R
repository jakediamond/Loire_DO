# 
# Purpose: To estimate metabolism at Dampierre in 1993-2018
# Author: Jake Diamond
# Date: November 11, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# Look at what data inputs are needed for MLE model
metab_inputs("mle", "data")

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
# Force the correct time zone
# df_t$datetime <- force_tz(df_t$datetime, "Etc/GMT+1")
# 
# compare <- left_join(df, df_t)
# plot(compare$temp[1000:1500], compare$temp.water[1000:1500])

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
# # It's nearly identical to Florentina's calculation, so use hers unless NA
# df$DO.sat_fill <- calc_DO_sat(temp.water = df$temp.water,
#                           pressure.air = calc_air_pressure(temp.air = df$temp,
#                                                            elevation = 118
#                                                            )
#                           )

# Florentina's DO.sat calculation
df$DO.sat <- ifelse(df$temp.water == 0,
                     0,
                     14.652 - 0.41022 * df$temp.water + 0.007991 * 
                       df$temp.water^2 - 0.000077774 * df$temp.water^3)
# Fill in gaps with 
# df$DO.sat <- ifelse(is.na(df$DO.sat),
#                     df$DO.sat_fill,
#                     df$DO.sat)
# df$DO.sat_fill 
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
  # drop_na(DO.sat) %>%
  mutate(time_frame = ifelse(year(solar.time) < 2001, 1, 2)) %>%
  select(DO.obs = filtered, temp.water, site, 
         light, depth, DO.sat, solar.time
         , time_frame
         )

# Create pre-post periods and nest data
df_n <- df %>%
  filter(site == "dampierre") %>%
  select(-site) %>%
  group_by(time_frame) %>%
  nest() %>%
  left_join(df_q %>%
              mutate(time_frame = ifelse(year(date) < 2001, 1, 2)) %>%
              group_by(time_frame) %>%
              nest() %>%
              rename(data_q = data))

# Estimate K from nighttime regression
k_test <- metab_night(
  specs(mm_name("night")), 
  data = filter(df, site == "dampierre") %>%
    select(-site, -time_frame))
x <- (get_params(k_test))
x %>%
  mutate(month = month(date)) %>%
  filter(K600.daily > 0,
         between(month, 6, 9)) %>%
  summarize(mean = mean(K600.daily, na.rm = T))
summary(x)
plot(x$date, x$K600.daily)

# Save it
saveRDS(k_test, "Data/K600_estimates_nighttime_regression_Dampierre")

# Get O connor and dobbins estimate of K600 daily for mle
df_d <- df_q %>%
  right_join(filter(df, site == "dampierre") %>%
              mutate(date = date(solar.time)) %>%
              group_by(date) %>%
              summarize(t = mean(temp.water, na.rm = T))) %>%
  mutate(v = 0.165*discharge.daily^0.275,
         d = 0.134*discharge.daily^0.4125,
         ka = 3.89*(v^0.5)/(d^1.5),
         s = 1801 - 120.1*t +3.782*t^2 -0.0476*t^3,
         K600.daily = (600/s)^-0.5 * ka) %>%
  select(date, K600.daily) 

summary(df_d)
# Compare O connor dobbins to nighttime regression to 
com <- left_join(df_d, x, by = "date")
plot(com$K600.daily.x, com$K600.daily.y)

# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a MLE model with both observation error and process error
# We will pool K600
bayes_mod <- mm_name(type = 'mle')
bayes_mod

# Quick clean
df_use <- df %>%
  filter(solar.time < ymd_hms("2018-12-30 04:09:58"))

df_d_use <- df_d %>%
  filter(date < ymd("2018-12-31"))

# Run the metabolism model on nested data ---------------------------------
mm_mle <- metab(specs = specs("mle"),
                data = filter(df_use, site == "dampierre") %>%
                  select(-site, -time_frame),
                data_daily = df_d)

saveRDS(mm_mle, "Data/Loire_DO/metab_mle")

# Inspect the model -------------------------------------------------------
mm <- predict_metab(mm_mle)

mm %>%
  filter(GPP > 0,
         ER < 0) %>%
  select(-msgs.fit, -warnings, -errors, -GPP, -ER) %>%
  mutate(period = ifelse(year(date) < 2001, 1, 2)) %>%
  pivot_longer(cols = contains("."), 
               names_to = c("flux", "range"),
               names_pattern = "([[:alnum:]]+).([[:alnum:]]+)") %>%
  pivot_wider(names_from = range, values_from = value) %>%
  left_join(mm %>%
              filter(GPP > 0,
                     ER < 0) %>%
              select(date, GPP, ER) %>%
              pivot_longer(cols = c(GPP, ER),
                           names_to = "flux",
                           values_to = "value")) %>%
  ggplot() + geom_point(aes(x = date, y = value, color = flux)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = flux), alpha = 0.4)  +
  facet_wrap(~period, scales = "free_x")

ggplot(data = filter(mm,
                     GPP > 0,
                     ER > 0), aes(x = date,
                      y = GPP)) + geom_point()

mm %>%
  mutate(year = year(date),
         month = month(date)) %>%
  select(year, date, GPP, ER, month) %>%
  filter(GPP > 0, ER < 0) %>%
  gather(flux, value, -year, -date, -month) %>%
  group_by(year, flux) %>%
  summarize(avg = mean(value, na.rm = T)) %>%
  ggplot(aes(x = year, y = avg, color = flux)) + geom_point()


kt <- get_params(mm_mle)
plot(kt$K600.daily, kt$ER.daily)
plot(kt$date, kt$K600.daily)
