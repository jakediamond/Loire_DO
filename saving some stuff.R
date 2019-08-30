# 
# Purpose: To analyze metabolism data for Loire River
# Author: Jake Diamond
# Date: August 28, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(streamMetabolizer)
library(lubridate)
library(dygraphs)

# Load data
mm2018 <- readRDS("C:/Users/jake.diamond/Dropbox/Projects/Loire_DO/Nouveau dossier/mm_results_2008_2018.rds")

# Dataframe of all metabolism estimates
df <- mm2018 %>%
  map(predict_metab) %>%
  map_df(bind_rows) %>%
  mutate(NPP = GPP + ER)

df %>%
  mutate(GPP = ifelse(GPP < 0, 0, GPP)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = GPP)) +
  geom_ribbon(aes(ymin = GPP.lower,
                  ymax = GPP.upper),
              alpha = 0.5,
              fill = "blue") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 20))

mm_2010 <- pluck(mm2018, 3)
mm_2013 <- pluck(mm2018, 6)
mm_2015 <- pluck(mm2018, 8)
mm_2017 <- pluck(mm2018, 10)
mm_2018 <- pluck(mm2018, 11)
saveRDS(mm_2013, file = "Data/Loire_DO/mm_2013_v1.rds")
saveRDS(df, file = "Data/Loire_DO/GPP_preds_v1.rds")
