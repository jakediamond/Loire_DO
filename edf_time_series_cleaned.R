# 
# Purpose: To plot cleaned time series data from EDF
# Author: Jake Diamond
# Date: September 5, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(dygraphs)
library(htmltools)

# Load data
df <- readRDS("Data/all_DO_cleaned")

# Daily amplitude, magnitude, and min for DO
df_day <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date,) %>%
  summarise(min = min(DO_use, na.rm = TRUE),
            max = max(DO_use, na.rm = TRUE),
            mag =  max - min) %>%
  ungroup() %>%
  gather(var, value, -date, -site)

# For later plot, make everything in the same year (2015)
df_day <- df_day %>%
  mutate(date2015 = ymd(format(df_day$date, "2015-%m-%d")),
         year = year(date))

x <- filter(df_day, is.na(date2015))

# Plot daily min and max by site
p <- ggplot(data = filter(df_day,
                          var != "mag"),
            aes(x = date,
                y = value,
                color = var)) +
  geom_point() +
  facet_grid(~ site, scales = "free"
             # ,labeller = label_wrap_gen(width=10)
             ) +
  # scale_x_date(date_breaks = "3 months",
  #              date_labels = "%m") +
  theme_bw() +
  scale_colour_viridis_d() +
  # theme(axis.title.x = element_blank(),
  #       legend.position = "bottom",
  #       legend.direction = "horizontal",
  #       legend.box.margin = margin(c(0,0,0,0), unit='cm'),
  #       legend.margin = margin(c(0,0,0,0), unit = "cm"),
  # #       panel.grid = element_blank()
  # # ) +
  # guides(color = guide_legend(nrow = 2)) + 
  # ylab("Mean daily value")

ggsave(plot = p,
       filename = "Figures/mean_daily_values_all_sites_both_periods.tiff",
       device = "tiff",
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

# Graph of hourly series for every site by year
p_annual <- ggplot(data = df,
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
df_dy <- df %>%
  select(datetime, site, DO_use) %>%
  spread(site, DO_use) %>%
  zoo::zoo(order.by = .$datetime)
df_dy$datetime <- NULL

dygraph(df_dy, main = "Dissolved oxygen time series") %>% 
  dyOptions(drawGrid = F,
            useDataTimezone = TRUE) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  dyRangeSelector()

dy_graphs <- list(
  dygraph(df_dy$belleville, 
          main = "Belleville",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$dampierre, 
          main = "Dampierre",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$chinon, 
          main = "Chinon",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$vienne, 
          main = "Vienne",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)) %>%
    dyRangeSelector()
)

htmltools::browsable(htmltools::tagList(dy_graphs))




# Interactive dygraphs min and max
# First need to get data in correct format
df_dy_mm <- df_day %>%
  spread(var, value) %>%
  select(-mag) %>%
  group_by(site) %>%
  nest() %>%
  mutate(ts = map(data, ~zoo::zoo(x = ., order.by = .$date)))

# Make a list of the graphs to display all at once
dy_graphs_min_max <- list(
  dygraph(pluck(df_dy_mm, 3, 1), 
          main = "Belleville",
          group = "df_dy_mm",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(pluck(df_dy_mm, 3, 3), 
          main = "Dampierre",
          group = "df_dy_mm",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(pluck(df_dy_mm, 3, 2), 
          main = "Chinon",
          group = "df_dy_mm",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(pluck(df_dy_mm, 3, 4), 
          main = "Vienne",
          group = "df_dy_mm",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)) %>%
    dyRangeSelector()
)

htmltools::browsable(htmltools::tagList(dy_graphs_min_max))
