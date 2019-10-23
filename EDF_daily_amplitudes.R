# 
# Purpose: To plot daily amplitudes of DO data from EDF
# Author: Jake Diamond
# Date: October 9, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(grid)

# Load DO data
df <- readRDS("Data/all_DO_cleaned")

# Load discharge daily data at Gien station (K4180010)
df_q <- readRDS("Data/dampierre_discharge_daily") %>%
  mutate(year = year(date),
         month = month(date),
         site = "Gien")

# Daily amplitude, magnitude, and min for DO
df_day <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date, year, month) %>%
  summarise(min = min(DO_use, na.rm = TRUE),
            max = max(DO_use, na.rm = TRUE),
            mag =  max - min) %>%
  ungroup() %>%
  gather(var, value, -date, -site, -year, -month)

# Plot boxplots
top_p <- ggplot(data = filter(df_day, 
                     var == "mag",
                     site != "vienne",
                     between(month, 5, 9)),
       aes(x = year,
           y = value,
           group = year)) +
  geom_violin() +
  stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red") +
  facet_grid(rows = vars(site), scales = "free_y") +
  stat_summary(fun.y=mean, geom="line", aes(group=1)) + 
  theme_bw() +
  xlab("") +
  ylab("Daily summer amplitude (mg/L)")


# Bottom discharge plot
bot_p <- ggplot(data = filter(df_q, 
                              between(year, 1993, 2018),
                              between(month, 5, 9)),
                aes(x = year,
                    y = discharge.daily,
                    group = year)) +
  geom_violin() +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red") +
  facet_grid(rows = vars(site), scales = "free_y") +
  stat_summary(fun.y=mean, geom="line", aes(group=1)) + 
  coord_cartesian(ylim=c(0, 1000)) +
  theme_bw() +
  xlab("") +
  ylab("Daily mean \n discharge (m3/s)")
bot_p  

plot_grid(top_p, bot_p, ncol = 1, rel_heights = c(3, 1))

ggsave(grid.draw(rbind(ggplotGrob(top_p), ggplotGrob(bot_p), size = "last")), 
       filename = "Figures/To share/amplitude_and_discharge_summary.tiff",
       device = "tiff",
       dpi = 300,
       width = 8,
       height = 6,
       units = "in")

