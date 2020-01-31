# 
# Purpose: To correct sensor drift
# Author: Jake Diamond
# Date: October 21, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(driftR)
library(tidyverse)
library(readxl)
library(scales)

# Load DO time series
df <- readRDS("Data/Headwaters_DO/DO_time_series")

# Load field measurements of DO
df_field <- read_xlsx("Data/Headwaters_DO/Field_Data.xlsx") %>%
  select(Site, DO_field = `DO (mg/L)`)

doise <- filter(df, Site == "Doise") %>%
  arrange(datetime) %>%
  mutate(DO = ifelse(between(datetime, 
                                ymd_hms("2019-07-13 05:00:00"),
                                ymd_hms("2019-07-22 05:00:00")) |
                       between(datetime, 
                               ymd_hms("2019-08-01 05:00:00"),
                               ymd_hms("2019-08-07 05:00:00")) |
                       between(datetime, 
                               ymd_hms("2019-09-15 05:00:00"),
                               ymd_hms("2019-09-25 05:00:00")),
                     NA,
                     DO)) %>%
  mutate(DO_sat = 14.652 - 0.41022 * temp + 0.007991 * 
                        temp^2 - 0.000077774 * temp^3,
         DO_per = DO *100 / DO_sat)
doise_plot_inter <- ggplot(data = doise,
       aes(x = datetime,
           y = DO_per)) +
  scale_x_datetime(limits = c(ymd_hms("2019-09-10 00:00:00"),
                                 ymd_hms("2019-09-30 00:00:00")),
                   breaks = "4 day",
                   labels = date_format("%b-%d", tz = "GMT+2")) +
  # scale_y_continuous(limits = c(4, 12)) +
  geom_line() + 
  theme_bw() +
  ylab("DO (% sat)") +
  xlab("") +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18))
doise_plot_inter         
ggsave(doise_plot_inter,
       filename = "Figures/doise_plot_intermittency.tiff",
       device = "tiff",
       dpi = 300,
       width = 10,
       height = 8,
       units = "in")


tor <- filter(df, Site == "Toranche Pontet") %>%
  arrange(datetime) %>%
  mutate(DO = ifelse(between(datetime, 
                             ymd_hms("2019-07-13 05:00:00"),
                             ymd_hms("2019-07-22 05:00:00")) |
                       between(datetime, 
                               ymd_hms("2019-09-30 03:00:00"),
                               ymd_hms("2019-10-03 15:00:00")) |
                       between(datetime, 
                               ymd_hms("2019-09-13 04:00:00"),
                               ymd_hms("2019-09-24 05:00:00")),
                     NA,
                     DO)) %>%
  mutate(DO_sat = 14.652 - 0.41022 * temp + 0.007991 * 
           temp^2 - 0.000077774 * temp^3,
         DO_per = DO *100 / DO_sat)
tor_plot_inter <- ggplot(data = tor,
                           aes(x = datetime,
                               y = DO_per)) +
  scale_x_datetime(limits = c(ymd_hms("2019-09-10 00:00:00"),
                              ymd_hms("2019-09-30 00:00:00")),
                   breaks = "4 day",
                   labels = date_format("%b-%d", tz = "GMT+2")) +
  # scale_y_continuous(limits = c(4, 12)) +
  geom_line() + 
  theme_bw() +
  ylab("DO (% sat)") +
  xlab("") +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18))
tor_plot_inter         
ggsave(tor_plot_inter,
       filename = "Figures/tor_plot_intermittency.tiff",
       device = "tiff",
       dpi = 300,
       width = 10,
       height = 8,
       units = "in")

write_csv2(df, "Data/Headwaters_DO.csv")

write_csv2(bind_rows(doise, tor), "Data/Headwaters_DO/intermittency_test.csv")
