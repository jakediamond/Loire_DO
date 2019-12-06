# 
# Purpose: To parse respiration for middle Loire data
# Author: Jake Diamond
# Date: September 24, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(quantreg)
library(broom)

# Load metab data
df_met <-readRDS("Data/Loire_DO/metabolism_results_all_years_constrainedK")

# Quantile regression on data by year
reg <- df_met %>%
  mutate(year = year(date)) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER)) %>%
  filter(between(month(date),5,10)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(model = map(data, ~rq(ER ~ GPP, 
                               tau = 0.9, 
                               data = ., 
                               na.action = na.omit),
                     ),
         results = map(model, tidy)) %>%
  unnest(results, .drop = TRUE)

ggplot(reg %>%
         filter(term == "GPP"),
       aes(x = year,
           y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high))
