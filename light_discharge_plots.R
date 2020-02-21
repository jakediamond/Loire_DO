# Purpose: To plot Figure X, hysteresis state plots, for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(changepoint)
library(imputeTS)
library(tidyverse)

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load some data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER)

df_light <- readRDS("Data/Loire_DO/light_dampierre_for_metab") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(light_sum = sum(light, na.rm = TRUE))
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Combine
df <- left_join(df_met, df_light) %>%
  left_join(df_q)

# Group by dischage
df <- df %>%
  mutate(brks = ntile(discharge.daily, 5))

# plot
ggplot(data = filter(df), 
       aes(x = light_sum, y = GPP,
                      color = year,
                      group = interaction(year, brks))) +
  geom_smooth(se = FALSE, method = "gam") +
  facet_wrap(~brks) +
  scale_color_viridis_c()

df %>%
  group_by(brks) %>%
  summarize(max = max(discharge.daily, na.rm = TRUE))

# Try to fit a monod model
df_mod <- df %>%
  filter(!is.na(brks),
         !is.na(GPP)) %>%
  group_by(brks) %>%
  nest() %>%
  mutate(start = list(list(a = 0.02)))
df_mod <- df_mod %>%
  filter(brks != 1) %>%
  mutate(mod = future_map2(data, start, ~nls(GPP ~ tanh(a * light_sum), 
                                  data = .x,
                                  start= .y)),
         tm = future_map(mod, tidy))


df_nls <- df %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  filter(brks == 5,
         between(month(date), 4, 9)) %>%
  drop_na(GPP, light_sum)


mmModel <- nls(GPP ~ Vm * light_sum / (K + light_sum), 
                  data = df_nls,
                  start = list(Vm = 25, K = 700))


mmModel <- nls(GPP ~ tanh(a * light_max), 
               data = df_nls,
               start = list(a = 0.01))

summary(mmModel)
coef(mmModel) 
confint(mmModel)
df_nls$resid <- resid(mmModel) 
x <- seq(min(df_nls$light_sum), max(df_nls$light_sum), length=100)
y <- predict(mmModel, list(light_sum=x))
plot(df_nls$light_sum, df_nls$GPP, las=1, pch=16, col=df_nls$year)
points(x, y, type='l', col='blue')

df_nls <- df_nls %>%
  mutate(bin = ntile(light_sum, 10))
df_nls_plot <- df_nls %>%
  group_by(bin) %>%
  summarize(light_bin = max(light_sum, na.rm = TRUE)) %>%
  left_join(df_nls) %>%
  group_by(year, light_bin) %>%
  summarize(mean = median(resid, na.rm = TRUE))
ggplot(data = df_nls_plot,
       aes(x = light_bin,
           y = mean,
           color = year,
           group = year)) +
  geom_point() +
  geom_path() +
  # stat_smooth(se = FALSE) +
  scale_color_viridis_c() 
