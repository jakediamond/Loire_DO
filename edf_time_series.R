# 
# Purpose: To plot time series data from EDF
# Author: Jake Diamond
# Date: June 7, 2019
# 

# Set working directory
setwd("//LY-LHQ-SRV/jake.diamond/Loire_DO")

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(dygraphs)

# Load data
df <- read_excel("Data/edf_1993_2000.xlsx",
                 sheet = 1,
                 col_names = c("code", "var", "datetime", "value",
                               "qc_code", "valid_code"),
                 skip = 1) %>%
  bind_rows(read_excel("Data/edf_2008_2018.xlsx",
                       sheet = 1,
                       col_names = c("code", "var", "datetime", "value",
                                     "qc_code", "valid_code"),
                       skip = 1))

# Some data cleaning
df <- df %>%
  separate(var, c("var", "site"), " de la ") %>%
  mutate(site = str_sub(site, end = -7),
         var = recode(var,
         `Température de l'eau horaire` = "T",
         `pH horaire` = "pH",
         `Oxygène dissous horaire` = "DO",
         `Conductivité horaire` = "SC"),
         datetime = dmy_hms(datetime, tz = "UTC")
  )

# Daily average
df_day <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date, var) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  ungroup()

# For later plot, make everything in the same year (2015)
df_day <- df_day %>%
  mutate(date2015 = ymd(format(df_day$date, "2015-%m-%d")),
         year = year(date))

x <- filter(df_day, is.na(date2015))

# Plot daily average by site and variable
p <- ggplot(data = df_day,
            aes(x = date2015,
                y = mean,
                color = as.factor(year))) +
  geom_line() +
  facet_grid(var ~ site, scales = "free",
             labeller = label_wrap_gen(width=10)) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%m") +
  theme_bw() +
  scale_colour_viridis_d(name = "Year",
                         direction = -1) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box.margin = margin(c(0,0,0,0), unit='cm'),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        panel.grid = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2)) + 
  ylab("Mean daily value")
p

ggsave(plot = p,
       filename = "Figures/mean_daily_values_all_sites_both_periods.tiff",
       device = "tiff",
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

# Interactive dygraphs
# First need to get data in correct format
bv <- df %>%
  filter(site == "Loire à Belleville Amont",
         var %in% c("DO", "SC")) %>%
  select(datetime, var, value) %>%
  spread(var, value) %>%
  zoo::zoo(order.by = .$datetime)
dygraph(bv, main = "Loire à Belleville Amont") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  dySeries("DO", axis=('y')) %>%
  dySeries("SC", axis=('y2'))


