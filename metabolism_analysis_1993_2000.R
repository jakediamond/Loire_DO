# 
# Purpose: To analyze metabolism 1993-2000 data for Loire River
# Author: Jake Diamond
# Date: August 14, 2019
# 

# Set working directory
setwd("Z:/Loire_DO/Data/Loire_DO")

# Load libraries
library(tidyverse)
library(streamMetabolizer)
library(lubridate)

# Load data
results <- readRDS("mm_results.rds")

# Dataframe of all metabolism estimates
df <- results %>%
  map(predict_metab) %>%
  map_df(bind_rows) %>%
  mutate(NPP = GPP + ER)

df2 <- df %>%
  gather(key = "type",
         value = "value",
         -msgs.fit,
         -warnings,
         -errors,
         -date) %>%
  mutate(year = year(date),
         julian = yday(date))

# Plot time series of GPP
df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = GPP)) +
  geom_ribbon(aes(ymin = GPP.lower,
                  ymax = GPP.upper),
              alpha = 0.1,
              color = "blue") +
  theme_classic()

# Plot all time series
df2 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value,
                color = type)) +
  facet_wrap(~type) +
  theme_classic()

# Plot cumulative years
df2 %>%
  filter(type %in% c("GPP", "ER", "NPP")) %>%
  group_by(type, year) %>%
  mutate(cum = order_by(julian, cumsum(replace_na(value, 0)))) %>%
  ggplot(aes(x = julian,
             y = cum,
             color = factor(year))) +
  geom_line() +
  facet_wrap(~type) + 
  scale_color_viridis_d(name = "Year") +
  theme_bw() + 
  theme(legend.position = c(0.08, 0.73)) +
  ylab(expression("Cumulative value (g"~O[2]~d^{-1}~m^{-2}*")")) +
  xlab("Julian Day")

# Get date of 50% GPP
x <- df2 %>%
  filter(type %in% c("GPP", "ER", "NPP")) %>%
  group_by(type, year) %>%
  mutate(cum = order_by(julian, cumsum(replace_na(value, 0))),
         rank = cum / max(cum)) %>%
  summarize(doy_50 = nth(julian, which.min(abs(rank -
                                0.5))))
  

  
  

# Daily metabolism predictions
predict_metab(df)
plot_metab_preds(df)

get_params(mm_93)
plot_DO_preds(df)
