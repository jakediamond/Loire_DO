# 
# Purpose: To summarize DO data for Loire headwaters
# Author: Jake Diamond
# Date: October 3, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidyverse)

# Load data and calculate percent saturation
df <- readRDS("Data/Headwaters_DO/DO_time_series") %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                          temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat)

# Get rid of bad data when sensors were buried or when out of water
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                 "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df <- df %>%
  filter(!(Site %in% flood_sites1 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-29 16:45"))),
         !(Site %in% flood_sites2 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-30 10:30"))),
         !(Site == "Coise aval Montrond" & between(datetime, 
                                                   ymd_hm("2019-08-06 16:00"),
                                                   ymd_hm("2019-09-11 11:30"))),
         !(Site == "Loise amont Doise Salt" & between(datetime, 
                                                   ymd_hm("2019-07-16 00:30"),
                                                   ymd_hm("2019-07-20 20:00"))),
         !(Site == "Loise aval Doise Salt" & between(datetime, 
                                                      ymd_hm("2019-08-06 19:00"),
                                                      ymd_hm("2019-08-30 10:15"))),
         !(Site == "Loise Essertine en Donzy" & between(datetime, 
                                                   ymd_hm("2019-07-20 22:15"),
                                                   ymd_hm("2019-07-23 15:30"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                   ymd_hm("2019-07-08 19:00"),
                                                   ymd_hm("2019-07-16 14:15"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                   ymd_hm("2019-07-24 23:00"),
                                                   ymd_hm("2019-08-29 15:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-12 19:45"),
                                               ymd_hm("2019-07-21 17:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-23 22:45"),
                                               ymd_hm("2019-07-29 01:00"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-31 03:45"),
                                               ymd_hm("2019-08-06 17:30"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-07-14 15:00"),
                                     ymd_hm("2019-07-20 17:15"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-08-03 20:15"),
                                     ymd_hm("2019-08-06 12:30")))
         
         )

# Calculate some metrics
df %>%
  mutate(date = date(datetime)) %>%
  # dplyr::filter(DO > 1) %>%
  group_by(Subwatershed, Subwatershed_order, Watershed, Watershed_order, Site, date) %>%
  summarize(mean_DO = mean(DO, na.rm = TRUE),
            max_DO = max(DO, na.rm = TRUE),
            min_DO = min(DO, na.rm = TRUE),
            amp_DO = max_DO - min_DO,
            mean_per = mean(DO_per, na.rm = TRUE),
            max_per = max(DO_per, na.rm = TRUE),
            min_per = min(DO_per, na.rm = TRUE),
            amp_per = max_per - min_per,
            mean_T = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter(amp < 4) %>%
  group_by(Site, Subwatershed, Subwatershed_order, Watershed, Watershed_order) %>%
  summarize(mean_DO = mean(mean_DO, na.rm = TRUE),
            max_DO = median(max_DO, na.rm = TRUE),
            min_DO = median(min_DO, na.rm = TRUE),
            amp_DO = median(amp_DO, na.rm = TRUE),
            mean_per = mean(mean_per, na.rm = TRUE),
            max_per = median(max_per, na.rm = TRUE),
            min_per = median(min_per, na.rm = TRUE),
            amp_per = median(amp_per, na.rm = TRUE),
            mean_T = mean(mean_T)) -> df_sum

# Save to disc
saveRDS(df_sum, "Data/Headwaters_DO/DO_summary")

# Quick look
df_sum %>%
  gather(msmt, val, amp) %>%
  ggplot(aes(x = Subwatershed_order, y = val)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Subwatershed) +
  scale_color_viridis_d() +
  theme_bw() +
  ylab('Mean daily DO amplitude')
