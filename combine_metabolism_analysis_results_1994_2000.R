# 
# Purpose: To analyze metabolism 1993-2000 data for Loire River
# Author: Jake Diamond
# Date: August 14, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(streamMetabolizer)

# Load data ---------------------------------------------------------------
# Load data for 1994
mm_1994 <- readRDS("Data/Loire_DO/mm_1994.rds")

# Dataframe of all 1995-2000 metabolism estimates
df_1994 <- predict_metab(mm_1994) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_1994) %>%
              select(date, K600.daily))

# Load data for 1995-2000 (list format)
mm_1995_2000 <- readRDS("Data/Loire_DO/mm_results.rds")

# Dataframe of all 1995-2000 metabolism estimates
df_1995_2000 <- mm_1995_2000 %>%
  map(predict_metab) %>%
  map_df(bind_rows) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_1995_2000 %>%
              map(get_params) %>%
              map_df(bind_rows) %>%
              select(date, K600.daily))

# Load data for 2008
mm_2008 <- readRDS("Data/Loire_DO/mm_2008.rds")

# Dataframe of 2008 metabolism estimates
df_2008 <- predict_metab(mm_2008) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_2008) %>%
              select(date, K600.daily))

# Load data for 2009-2011
mm_2009_2011 <- readRDS("Data/Loire_DO/mm_2009_2011.rds")

# Dataframe of 2009-2011 metabolism estimates
df_2009_2011 <- predict_metab(mm_2009_2011) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_2009_2011) %>%
              select(date, K600.daily))

# Load data for 2012-2015
mm_2012_2015 <- readRDS("Data/Loire_DO/mm_2012_2015.rds")

# Dataframe of 2012-2015 metabolism estimates
df_2012_2015 <- predict_metab(mm_2012_2015) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_2012_2015) %>%
              select(date, K600.daily))

# Load data for 2016
mm_2016 <- readRDS("Data/Loire_DO/mm_2016.rds")

# Dataframe of 2016 metabolism estimates
df_2016 <- predict_metab(mm_2016) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_2016) %>%
              select(date, K600.daily))

# Load data for 2017-2018
mm_2017_2018 <- readRDS("Data/Loire_DO/mm_2017_2018.rds")

# Dataframe of 2017-2018 metabolism estimates
df_2017_2018 <- predict_metab(mm_2017_2018) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(get_params(mm_2017_2018) %>%
              select(date, K600.daily))

# Combine all data
df <- bind_rows(df_1994, df_1995_2000, df_2008, df_2009_2011,
          df_2012_2015, df_2016, df_2017_2018)

# Save all this data
saveRDS(df, file = "Data/Loire_DO/metab_results_1994_2018.rds")