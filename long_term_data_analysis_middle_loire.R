# 
# Purpose: To summarize long-term data for middle Loire River
# Author: Jake Diamond
# Date: October 7, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(broom)
# library(ggpubr)
# library(tabulizer)
# library(readxl)

# Load long term wq data
df <- readRDS("Data/Headwaters_DO/all_longterm_wq_data")

# Bit of cleaning
df <- df %>%
  drop_na() %>%
  rename(DOC = COD,
         TKN = NKj,
         SiO2 = SIO,
         temp = TEMP_EAU,
         SC = COND,
         TP = PTO,
         DO = O2,
         year = Annee,
         month = Mois,
         day = Jour)

# Just middle Loire sites, join with discharge data
df_mid <- filter(df, site_no %in% c("04048000",
                                    "04048100",
                                    "04049000",
                                    "04049850",
                                    "04050500",
                                    "04050500",
                                    "04050550",
                                    "04051000"
                                    )) %>%
  left_join(readRDS("Data/dampierre_discharge_daily"))

# Data analysis -----------------------------------------------------------
# Some data summaries. Change all negatives to NA
df_mid <- df_mid %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  gather(solute, value, temp:Mg)

(ggplot(data = df_mid) + 
  geom_point(aes(x = date,
                 y = value,
                 color = month)) +
  facet_wrap(~solute, scales = "free_y") +
  scale_color_viridis_c(name = "Month") + 
  theme_bw() + 
  xlab("") +
  ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_time_series_all_sites.tiff",
         device = "tiff",
         dpi = 300,
         width = 30,
         height = 19.4,
         units = "cm")

# Clean data for NAs to plot CQ
df_mid_clean <- df_mid %>%
  filter_at(vars(discharge.daily, value), all_vars(!is.na(.)))

(ggplot(data = df_mid_clean) + 
    geom_point(aes(x = discharge.daily,
                   y = value,
                   color = year)) +
    scale_y_log10() +
    scale_x_log10() +
    facet_wrap(~solute, scales = "free_y") + 
    scale_color_viridis_c() +
    theme_bw() + 
    xlab("Discharge (m3/s)") +
    ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_CQ_all_sites.tiff",
         device = "tiff",
         dpi = 300,
         width = 30,
         height = 19.4,
         units = "cm")

df_mid %>%
  filter(solute == "PO4") %>%
  group_by(year, solute) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  tail()

(ggplot(data = df_mid, aes(x = year, y = value)) + 
    stat_summary(fun.y = mean, geom = "point") + 
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
    facet_wrap(~solute, scales = "free_y") +
    theme_bw() + 
    xlab("") +
    ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_time_series_annual_all_sites.tiff",
         device = "tiff",
         dpi = 300,
         width = 30,
         height = 19.4,
         units = "cm")

df_mid_pl <- df_mid_clean %>%
  group_by(site_no, solute) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(log(.$value)~log(.$discharge.daily)))) %>%
  mutate(glance_lm = map(mod, glance),
         rsq =map_dbl(glance_lm, "r.squared"),
         tidy_lm = map(mod, tidy)) %>%
  unnest(tidy_lm)


# Quick plot of BOD vs ChlA
df_mid_clean %>% 
  filter(solute %in% c("DBO5", "CHLA")) %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  ggplot(aes(x = CHLA, y = DBO5, 
             color = as.factor(year),
             group = year)) +
  geom_point() +
  # scale_color_viridis_c() +
  geom_smooth(method = "lm")
