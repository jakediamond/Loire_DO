# 
# Purpose: To analyze metabolism 2008-2018 data for Loire River
# Author: Jake Diamond
# Date: August 23, 2019
# 

# Set working directory
setwd("C:/Users/diamo/Dropbox/Projects/Loire_DO")

# Load libraries
library(tidyverse)
library(streamMetabolizer)
library(lubridate)
library(dygraphs)

# Load data
mm_results <- readRDS("Nouveau dossier/mm_results_2008_2018.rds")

# Dataframe of all metabolism estimates
df <- mm_results %>%
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
  mutate(GPP = ifelse(GPP < 0, 0, GPP)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = GPP)) +
  geom_ribbon(aes(ymin = GPP.lower,
                  ymax = GPP.upper),
              alpha = 0.5,
              fill = "blue") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 20))

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

df %>%
  ggplot() +
  geom_point(aes(x = GPP,
                 y = ER))

# Daily metabolism predictions
# Dataframe of all metabolism estimates
df_DO <- mm_results %>%
  map(predict_DO) %>%
  map_df(bind_rows)
df_DO <- predict_DO(mm_2016)
mm_results %>%
  map(plot_DO_preds)

bv_do <- df_DO %>%
  select(DO.obs, DO.mod) %>%
  zoo::zoo(order.by = df_DO$solar.time)

x <- pluck(mm_results, 1)
plot_DO_preds(mm)
s <- get_mcmc(mm)
k <- get_fit(mm)$daily %>%
  select(ends_with("Rhat"))
pairs(s)

dygraph(bv_do, main = "Loire à Dampierre") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  # dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  dySeries("DO.mod", axis=('y')) %>%
  dySeries("DO.obs", axis=('y'), drawPoints = TRUE)

df_pars <- mm_results %>%
  map(get_params) %>%
  map_df(bind_rows)
df_pars <- get_params(mm)
ggplot(data = df_pars,
       aes(x = K600.daily,
           y = ER.daily)) + 
  geom_point() +
  scale_x_continuous(limits = c(0,1))

traceplot(s)
