# 
# Purpose: To analyze metabolism 1993-2000 data for Loire River
# Author: Jake Diamond
# Date: September 19, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(streamMetabolizer)

# Load data ---------------------------------------------------------------
# Load data for 1993-2000 (list format)
mm_1993_2000 <- readRDS("Data/metab_constrainedK_1993_2000")

# Dataframe of all 1995-2000 metabolism estimates
df_1993_2000 <- mm_1993_2000[1:2, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_1993_2000[1:2, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  left_join(mm_1993_2000[1:2, ] %>%
              mutate(mcmc = map(mm, get_fit)) %>%
              mutate(err = map(mcmc, ~pluck(., 2))) %>%
              unnest(err) %>%
              group_by(date) %>%
              summarize(mean_proc_err = mean(abs(err_proc_iid_mean),
                                             na.rm = TRUE)))

# Load data for 2008-2013
mm_2008_2013 <- readRDS("Data/Loire_DO/metab_constrainedK_2008_2013")

# Dataframe of 2008-2013 metabolism estimates
df_2008_2013 <- mm_2008_2013[1, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_2008_2013[1, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  left_join(mm_2008_2013[1, ] %>%
              mutate(mcmc = map(mm, get_fit)) %>%
              mutate(err = map(mcmc, ~pluck(., 2))) %>%
              unnest(err) %>%
              group_by(date) %>%
              summarize(mean_proc_err = mean(abs(err_proc_iid_mean),
                                             na.rm = TRUE)))

# Load data for 2012-2015
mm_2012_2015 <- readRDS("Data/Loire_DO/metab_constrainedK_2012_2015")

# Dataframe of 2008-2013 metabolism estimates
df_2012_2015 <- mm_2012_2015[2, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_2012_2015[2, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  left_join(mm_2012_2015[2, ] %>%
              mutate(mcmc = map(mm, get_fit)) %>%
              mutate(err = map(mcmc, ~pluck(., 2))) %>%
              unnest(err) %>%
              group_by(date) %>%
              summarize(mean_proc_err = mean(abs(err_proc_iid_mean),
                                             na.rm = TRUE)))

# Load data for 2008, 2016-2018
mm_2008_2016_2018 <- readRDS("Data/Loire_DO/metab_constrainedK_2016_2018_also2008_weird")

# Dataframe of 2008 metabolism estimates
df_2008_2016_2018 <- mm_2008_2016_2018[1, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_2008_2016_2018[1, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  left_join(mm_2008_2016_2018[1, ] %>%
              mutate(mcmc = map(mm, get_fit)) %>%
              mutate(err = map(mcmc, ~pluck(., 2))) %>%
              unnest(err) %>%
              group_by(date) %>%
              summarize(mean_proc_err = mean(abs(err_proc_iid_mean),
                                             na.rm = TRUE)))
# Combine all data --------------------------------------------------------
df <- bind_rows(df_1993_2000, df_2008_2016_2018 %>%
                  filter(year(date) != 2008), 
                df_2008_2013 %>%
                  filter(!(year(date) %in% c(2012, 2013))),
                df_2012_2015)

# Save all this data
saveRDS(df, file = "Data/Loire_DO/metab_results_1993_2018_constrainedK")
