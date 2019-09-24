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
df <- readRDS("Data/Loire_DO/metab_results_1993_2018_constrainedK")

# Quantile regression on data by year
reg <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(model = map(data, ~rq(ER ~ GPP, 
                               tau = 0.7, 
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
