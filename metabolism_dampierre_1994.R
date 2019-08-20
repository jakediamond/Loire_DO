# 
# Purpose: To estimate metabolism at Dampierre in 1994
# Author: Jake Diamond
# Date: July 22, 2019
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

# Data load and clean -----------------------------------------------------
# Load data
df <- read_excel("Data/Loire_DO/dampierre_1994_prepared.xlsx",
                 sheet = 1)

# Some data cleaning
df$datetime <- ymd_hms(df$datetime, tz = "UTC")

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Convert to solar time at Gien station
df$solar.time <- calc_solar_time(df$datetime, longitude = 2.5)

# Get rid of datetime
df$datetime <- NULL

# Get rid of discharge unless pooling
df$discharge <- NULL

# Make sure it's a dataframe and not a tbl_df
df <- as.data.frame(df)

# Caclculate light (not needed now, used conversion from W/m2)
# df$light <- calc_light(solar.time = df$solartime,
#                        latitude = 47.7,
#                        longitude = 2.5)

# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_name <- mm_name(type = 'bayes', 
                      pool_K600 = 'normal', 
                      err_obs_iid = TRUE, 
                      err_proc_iid = TRUE)
bayes_name

# Set the specifications
bayes_specs <- specs(bayes_name)
bayes_specs


# Fit the model -----------------------------------------------------------
mm <- metab(bayes_specs, data = df)


# Inspect the model -------------------------------------------------------
mm

# Daily metabolism predictions
predict_metab(mm)
plot_metab_preds(mm)

get_params(mm)
plot_DO_preds(mm)

# mcmc <- get_mcmc(mm)
# rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
bayes_npp_noq <- predict_metab(mm)
bayes_npp_noq_p <- plot_metab_preds(mm)
bayes_npp_noq_p


bayes_npp_q <- predict_metab(mm)
bayes_npp_q_p <- plot_metab_preds(mm)


bayes_npp_q <- readRDS("dampierre_1994_metabolism_bayes_pooling_result")

saveRDS(bayes_npp_noq, 
        file = "dampierre_1994_metabolism_bayes_no_pooling_result")
saveRDS(bayes_npp_q, 
        file = "dampierre_1994_metabolism_bayes_pooling_result")
saveRDS(mm, 
        file = "dampierre_1994_metabolism_bayes_pooling")

com <- bayes_npp_noq %>%
  # select(GPP, ER, date) %>%
  mutate(cGPP = cumsum(GPP),
         cGPPe = 
         cER = cumsum(ER),
         cNPP = cGPP + cER,
         method = "no_pool") %>%
  select(-GPP, -ER) %>%
  gather(type, value, -date, -method) %>%
  bind_rows(bayes_npp_q %>%
              select(GPP, ER, date) %>%
              mutate(cGPP = cumsum(GPP),
                     cER = cumsum(ER),
                     cNPP = cGPP + cER,
                     method = "pool") %>%
              select(-GPP, -ER) %>%
              gather(type, value, -date, -method))
p_comp <- ggplot(data = com,
                 aes(x = date,
                     y = value,
                     color = method)) +
  geom_line() +
  facet_grid(rows = vars(type), scales = "free_y")
p_comp

y <- get_params(mm)
ggplot(data = y,
       aes(x = date,
           y = K600.daily)) + 
  geom_line() + 
  geom_errorbar()
x <- bayes_npp_q %>%
    summarize(avgER = mean(ER),
            avgGPP = mean(GPP))
