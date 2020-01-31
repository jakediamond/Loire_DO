# 
# Purpose: To plot metabolism results for Loire
# Author: Jake Diamond
# Date: January 13, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("D:/jake.diamond/Loire_DO")

# Load libraries
# library(streamMetabolizer)
library(lubridate)
library(tidyverse)

# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load cleaned DO data
df_met_l <- df_met %>%
  ungroup() %>%
  # select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  gather(key, value, -date) %>%
  mutate(type_plot = recode(key,
                            `ER` = "ER~(g~O[2]~m^{-2}~d^{-1})",
                            `GPP` = "GPP~(g~O[2]~m^{-2}~d^{-1})",
                            `NPP` = "NPP~(g~O[2]~m^{-2}~d^{-1})",
                            `K600.daily` = "k[600]~(d^{-1})"))

df_met_l$type_plot <- factor(df_met_l$type_plot,
                             levels = c("GPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "ER~(g~O[2]~m^{-2}~d^{-1})",
                                        "NPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "k[600]~(d^{-1})"))

# Plot the data
(ggplot(data = df_met_l,
        aes(x = date,
            y = value)) +
    geom_point(alpha = 0.4, size = 0.8) +
    facet_grid(rows = vars(type_plot), scales = "free_y",
               labeller = label_parsed) + 
    scale_x_date(date_breaks = "1 years",
                 limits = c(ymd("1993-01-01"),
                            ymd("2018-12-31")),
                 date_labels = "%Y") +
    theme_bw(base_size=7) +
    theme(panel.grid.minor = element_blank()) + 
    xlab("") +
    ylab("")) %>%
  ggsave(filename = "Figures/Middle_Loire/metabolism_pool_facets_pool.png",
         device = "png",
         dpi = 300,
         width = 18.4,
         height = 9.2,
         units = "cm")
