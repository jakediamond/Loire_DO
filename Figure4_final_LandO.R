# 
# Purpose: To do early warning analysis of Middle Loire data
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(earlywarnings)
library(imputeTS)
library(tidyverse)
# set to run in parallel
future::plan(multiprocess)

# Set the plotting theme
theme_set(theme_bw(base_size=9)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load raw metabolism data
df <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  dplyr::select(-GPP, -ER, -NPP, -K600.daily)


# Convert do data to time series format
df_ts <- df %>%
    arrange(date) %>%
    na.trim() %>%
    as.data.frame(.) %>%
    na_kalman(.) %>%
    xts(x = .[, "NEP"],
        order.by = .[, "date"])


#  calculate ew on raw data
df_ew <- generic_ews(df_ts, winsize = 16)

# function to get time indices related to datetimes
ts_data <- tibble(date = index(df_ts),
                   NEP = as.numeric(df_ts)) %>%
  mutate(timeindex = row_number())

# Plot data for early warning signals
ew_plot_data <- left_join(ts_data, df_ew,
                          by = c("timeindex")) %>%
  pivot_longer(cols = -c(timeindex, date))

# rename for plotting
plotnames <- tibble(name = c("ar1", "acf1", "densratio", "kurt", "cv",
                             "returnrate", "sd", "sk",
                             "NEP"),
                    plotname = c("ar(1)", "acf(1)", "density~ratio", "kurtosis", "cv",
                                 "return~rate", "standard~deviation", "skewness",
                                 "NEP~(g~O[2]~m^{-2}~d^{-1})"))
left_join(ew_plot_data, plotnames, by = "name") -> ew_plot_data
ew_plot_data$plotname <- factor(ew_plot_data$plotname,
                            levels = c("NEP~(g~O[2]~m^{-2}~d^{-1})",
                                       "ar(1)",
                                       "standard~deviation",
                                       "skewness"))
levels(ew_plot_data$plotname) <- c("NEP~(g~O[2]~m^{-2}~d^{-1})",
                                  "ar(1)",
                                  "standard~deviation",
                                  "skewness")


# Plot the data
p_ew <- ggplot() + 
  geom_line(data = filter(ew_plot_data,
                          name %in% c("ar1", "sk", "sd", "NEP")),
            aes(x = date, y = value)) +
  scale_x_date(date_breaks = "3 years",
               limits = c(ymd("1993-01-01"), ymd("2019-01-01")),
               date_labels = "%Y") +
  annotate(geom = "rect",
           xmin = ymd("2013-09-01"),
           xmax = ymd("2014-01-01"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
    annotate(geom = "rect",
             xmin = ymd("2003-07-01"),
             xmax = ymd("2004-10-01"),
             ymin = -Inf,
             ymax = Inf,
             alpha = 0.4,
             fill = "#008B45FF") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~plotname, scales = "free_y", 
             strip.position = "left", ncol = 1, labeller = label_parsed)
p_ew





ggsave(plot = p_ew,
       filename = "Figures/Figure4_final_LandO.png",
         device = "png",
         dpi = 300,
         width = 88,
         height = 120,
         units = "mm")

skewness(ts_data$NEP)
