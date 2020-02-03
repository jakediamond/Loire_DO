# 
# Purpose: To plot time series data from EDF
# Author: Jake Diamond
# Date: June 7, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(dygraphs)
library(htmltools)

# Load data
df <- read_excel("Data/EDF/edf_1993_2000.xlsx",
                 sheet = 1,
                 col_names = c("code", "var", "datetime", "value",
                               "qc_code", "valid_code"),
                 
                 skip = 1) %>%
  mutate(datetime = dmy_hms(datetime, tz = "UTC")) %>%
  bind_rows(read_excel("Data/EDF/edf_2001_2007.xlsx",
                       sheet = 1,
                       col_names = c("code", "var", "datetime", "value",
                                     "qc_code", "valid_code"),
                       skip = 1)) %>%
  bind_rows((read_excel("Data/EDF/edf_2008_2018.xlsx",
                       sheet = 1,
                       col_names = c("code", "var", "datetime", "value",
                                     "qc_code", "valid_code"),
                       skip = 1)) %>%
              mutate(datetime = dmy_hms(datetime, tz = "UTC"))) %>%
  bind_rows((read_excel("Data/EDF/edf_2008_2018.xlsx",
                       sheet = 2,
                       col_names = c("code", "var", "datetime", "value",
                                     "qc_code", "valid_code"),
                       skip = 1)) %>%
              mutate(datetime = dmy_hms(datetime, tz = "UTC")))

# Some data cleaning
df <- df %>%
  separate(var, c("var", "site"), " de la ") %>%
  mutate(site = str_sub(site, end = -7),
         var = recode(var,
         `Température de l'eau horaire` = "T",
         `pH horaire` = "pH",
         `Oxygène dissous horaire` = "DO",
         `Conductivité horaire` = "SC")
  )


# Site name cleaning
df <- df %>%
  mutate(site = case_when(grepl("Vienne", site) ~ "vienne",
                           grepl("Belleville", site) ~ "belleville",
                           grepl("Chinon", site) ~ "chinon",
                           grepl("Dampierre", site) ~ "dampierre",
                           grepl("Saint", site) ~ "saint_laurent"))

# Write to file
saveRDS(df, file = "Data/all_DO_data")

# Daily average, magnitude, and min
df_day <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date, var) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            mag = max(value, na.rm = TRUE) - min) %>%
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

ggsave(plot = p,
       filename = "Figures/mean_daily_values_all_sites_all_years.tiff",
       device = "tiff",
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

# Graph of 15 minute time series for every site by year
df_annual <- df %>% 
  mutate(year = year(datetime)) %>%
  filter(var == "DO") %>%
  select(datetime, var, value, site, year) %>%
  spread(var, value)

p_annual <- ggplot(data = df_annual,
                   aes(x = datetime,
                       y = DO)) + 
  geom_line() +
  facet_grid(rows = vars(site)) +
  scale_x_datetime(date_breaks = "3 months",
               date_labels = "%m") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank()
  ) +
  ylab(expression("DO (mg "*L^-1*")"))

p_annual <- ggplotly(p_annual)
p_annual


# Interactive dygraphs
# First need to get data in correct format
df_dy <- df_annual %>%
  select(datetime, site, DO) %>%
  spread(site, DO) %>%
  zoo::zoo(order.by = .$datetime)
df_dy$datetime <- NULL

dygraph(df_dy, main = "Dissolved oxygen time series") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  dyRangeSelector()

dy_graphs <- list(
  dygraph(df_dy$belleville, 
          main = "Belleville",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE),
  
  dygraph(df_dy$dampierre, 
          main = "Dampierre",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE),
  
  dygraph(df_dy$chinon, 
          main = "Chinon",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE),
  
  dygraph(df_dy$vienne, 
          main = "Vienne",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE) %>%
    dyRangeSelector()
)

htmltools::browsable(htmltools::tagList(dy_graphs))
