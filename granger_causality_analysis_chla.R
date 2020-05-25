# 
# Purpose: To plot granger causality analysis for middle loire River GPP, ER
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lmtest)
library(furrr)
library(tidyverse)
library(tseries)

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm"))))

# Load data
df_chla <- read_excel("Data/Chla_GPP_11 stations_Gien to Villandry_et HFCM.xlsx", sheet = 4)
# Granger causality ER GPP ------------------------------------------------
# Granger causality ER GPP
df_met_n <- df_chla %>%
  ungroup() %>%
  # dplyr::select(-time_frame) %>%
  mutate(#GPP2 = ifelse(GPPmoymob10 < 0, NA, GPPmoymob10),
         chla = ifelse(chlainterp < 0, NA, chlainterp),
         year = year(dateinterp),
         week = week(dateinterp),
         month = month(dateinterp)) %>%
  filter(between(month, 3, 11)) %>%
  group_by(year) %>%
  na.trim(.) %>%
  as.data.frame(.) %>%
  na_kalman(., maxgap = 3) %>%
  # mutate(gpp = if_else(is.na(GPP), 0, GPP),
  #        er = if_else(is.na(ER), 0, ER)) %>%
  as_tibble(.) %>%
  group_by(year) %>%
  mutate(GPPr = rollapplyr(GPP, width =3, FUN = mean, na.rm = TRUE, fill = NA) ,
         chlar = rollapplyr(chla, width =3, FUN = mean, na.rm = TRUE, fill = NA)) %>%
  nest()

# test if data is stationarity, do granger causality
df_gc <- df_met_n %>%
  # mutate(stat = future_map(data, ~adf.test(.$chla)$p.val))
  mutate(gc_er_gpp = future_map(data, ~grangertest(.$chlar ~ .$GPPr, 
                                                   order = 1))) %>%
  unnest(gc_er_gpp)

p_grang <- ggplot(data = na.omit(df_gc),
       aes(x = year,
           y = `Pr(>F)`)) +
  geom_point() +
  # geom_rect(aes(xmin = 2011,
  #               xmax = 2013,
  #               ymin = -Inf,
  #               ymax = Inf),
  #           alpha = 0.4,
  #           fill = "dark blue") +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1993, 2018, 2),
                     limits = c(1993, 2018)) +
  xlab("") +
  ylab(expression(chla[summer]*`~`*GPP[summer]~Granger~causality~p-val))
p_grang

ggsave(plot = p_grang, 
       filename = "Figures/Middle_Loire/granger_causality_gpp_chla.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")
