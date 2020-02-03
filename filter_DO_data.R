# 
# Purpose: To smooth DO time-series for middle Loire, reduce outliers
# Author: Jake Diamond
# Date: September 5, 2019
# 

# Set working directory
<<<<<<< HEAD
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("D:/jake.diamond/Loire_DO")
=======
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
>>>>>>> b932b3b7e82798c70c22dee711ea77fd6bc70bba

# Load libraries
library(zoo)
library(signal)
library(imputeTS)
library(dygraphs)
library(tidyverse)
library(lubridate)
library(furrr)

# Read in all DO data
df <- readRDS("Data/all_DO_data")

# Only get DO data and nest data by site, 
# Split into the two periods 1993-2000 and 2008-2018
# rename DO_obs for observed values
df_n <- df %>%
  dplyr::filter(var == "DO") %>%
  select(site, datetime, value) %>%
  rename(DO_obs = value) %>%
  mutate(period = ifelse(between(datetime,
                                 ymd_hms("1993-01-01 00:00:00"),
                                 ymd_hms("2001-01-01 00:00:00")),
                         1,
                         2)
         ) %>%
  group_by(site, period) %>%
  nest()

# Remove original data to keep workspace clean
rm(df)

# Define functions --------------------------------------------------------
# Data cleaning function, finds anomalies/jumps/replaces with NA
# Can prescribe the trend for the anomaly decomposition (default 7 days),
# the probability limit for delta_DO for detecting "bad" jumps (default 0.95),
# the multiplier for delta_DO to detect the jump (default 1.5)
# also a minimum threshold for expected DO (default 1)
clean_fun <- function(data,
                      trend = "7 days",
                      prob = 0.95,
                      mult = 1.5,
                      minDO = 2){
  # Order data
  data <- data[with(data, order(datetime)), ]
  # Create hourly time series 
  hr_ts <- data.frame(datetime = seq(floor_date(min(data$datetime), 
                                                unit = "year"),
                                     ceiling_date(max(data$datetime), 
                                                  unit = "day"),
                                     by = "hour"))
  # Combine with data so that every hour has a data point
  data <- right_join(data, hr_ts)
  # First subset data to get rid of NAs at the front or back end
  data <- na.trim(data)
  # Then interpolate all NAs so that there are none with Stineman method
  data$DO_int <- na_interpolation(data$DO_obs, option = "stine")

  # Then determine anomalies/outliers with the gesd method
  # Use a trend of 7 days to catch major anomalies more easily
  # data <- data %>%
  #   time_decompose(DO_int, frequency = "day", trend = trend) %>%
  #   anomalize(remainder, method = "gesd") %>%
  #   time_recompose() %>%
  #   select(datetime, anomaly) %>%
  #   right_join(data)
  # Calculate upper 95% average delta DO by month and year
  # Use this to set a bound on what to expect for big jumps
  del_do <- data %>%
    mutate(month = month(datetime),
           year = year(datetime),
           ddo = DO_int - lag(DO_int)) %>%
    group_by(year, month) %>%
    summarize(ddo_lim = quantile(abs(ddo), 
                                  probs = prob, 
                                  na.rm = TRUE))
  # Make the data NA where there are big jumps, or just wrong data (anomalies)
  # If the data jump more than 1.5x the 95% value for that time period
  # If DO.obs is less than 2 mg/L (not really possible in this river)
  # If there was an anomaly from decomposition method (but not 
  # for summertime or very specific period in 1993 with weird spring values)
  data <- data %>% 
    mutate(month = month(datetime),
           year = year(datetime)) %>%
    left_join(del_do %>%
                select(year, month, ddo_lim)) %>%
    mutate(ddo = DO_int - lag(DO_int),
           DO = ifelse(abs(ddo) > mult * ddo_lim | 
                         is.na(ddo) | 
                         DO_int <= minDO #|
                         # (anomaly == "Yes" &
                         #    !(month(datetime) %in% c(6,7,8,9) |
                         #        between(datetime, 
                         #                ymd_hms("1993-03-01 00:00:00"),
                         #                ymd_hms("1993-04-01 00:00:00"))
                         #      )
                         #  )
                       , NA,
                       DO_int
           )
    )
}

# Define lowpass filter function
# Can prescribe cutoff_frequency for low pass bandwidth (default 0.15 
# for hourly data)
lowpass_fun <- function(data, 
                        cutoff_frequency = 0.15) {
  # Re-interpolate all NAs so that there are none with Stineman method
  data$DO_an_int <- na_interpolation(data$DO, option = "stine")
  # Order the data, just in case
  data <- data[with(data, order(datetime)),]
  # Sampling rate [s^-1]
  sr <- 1 / (as.numeric(data$datetime[2] - data$datetime[1]) * 60)
  # Nyquist frequency = half the sampling rate
  nyq <- sr / 2
  # Cutoff frequency (hours^-1)
  cutoff <- 1 / (cutoff_frequency * 60 * 60)
  # Normalized cutoff frequency for Butterworth filter
  W <- cutoff / nyq
  # Butterworth low-pass filter, digital, 2nd order
  myfilter <- butter(2, W, type = 'low', plane = 'z')
  # Forward-reverse filter to remove phase-shift 
  # associated with Butterworth filter (must be in vector-form)
  vec <- as.vector(data$DO_an_int)
  filtered <- filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(datetime)), ]
  rem <- sr / cutoff
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}

# Apply functions ---------------------------------------------------------
# Clean the data and use the lowpass filter
df_n <- df_n %>%
  mutate(clean = future_map(data, clean_fun),
         filt = future_map(clean, lowpass_fun))

# Get all in one dataframe
df_DO <- unnest(df_n, filt)

# Finally, add  back NAs for large chunks of missing data (i.e., >=1 day)
# because the filter can't adequately fill these gaps
df_DO <- df_DO %>%
  group_by(site, year) %>%
  mutate(na_check = 
           {res = rle(is.na(DO_obs));
           rep(res$values * res$lengths, res$lengths)},
         DO_use = ifelse(na_check > 24,
                         NA,
                         filtered)) %>%
  ungroup() %>%
  select(-data, -clean)

# Reduce size
head(df_DO)
df_DO <- select(df_DO, -clean, -data)
# Save data
saveRDS(df_DO, "Data/all_DO_cleaned")

