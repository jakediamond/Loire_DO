# 
# Purpose: To analyze metabolism 1994-2018 data for Loire River
# Author: Jake Diamond
# Date: August 29, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(dygraphs)

# Load data
df <- readRDS("Data/Loire_DO/metab_results_1994_2018.rds")

df %>%
  mutate(year = year(date)) %>%
  filter(year > 1996) %>%
  bind_rows(predict_metab(mm) %>%
              mutate(NPP = GPP + ER) %>%
              left_join(get_params(mm) %>%
                          select(date, K600.daily))) -> df

# Get data in long format
df_l <- df %>%
  select(date, GPP, ER, NPP, K600.daily) %>%
  mutate(ER = ifelse(ER >=0 | is.na(ER), 0, ER),
         GPP = ifelse(GPP < 0 | is.na(GPP), 0, GPP),
         NPP = GPP + ER,
         year = year(date),
         julian = yday(date)) %>%
  gather(flux, value, -date, -year, -julian)

# Plot time series of GPP
df %>%
  mutate(GPP = ifelse(GPP < 0, 0, GPP)) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = ER)) +
  # geom_ribbon(aes(ymin = GPP.lower,
  #                 ymax = GPP.upper),
  #             alpha = 0.5,
  #             fill = "blue") +
  theme_classic()

# Plot all time series
df_l %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value,
                color = type)) +
  facet_wrap(~type) +
  theme_classic()

# Plot cumulative years
df_l %>%
  filter(flux %in% c("GPP", "ER", "NPP")) %>%
  group_by(flux, year) %>%
  mutate(cum = order_by(julian, cumsum(replace_na(value, 0)))) %>%
  ggplot(aes(x = julian,
             y = cum,
             color = factor(year))) +
  geom_line() +
  facet_wrap(~flux) + 
  scale_color_viridis_d(name = "Year") +
  theme_bw() + 
  theme(legend.position = c(0.08, 0.73)) +
  ylab(expression("Cumulative value (g"~O[2]~d^{-1}~m^{-2}*")")) +
  xlab("Julian Day")

# Calculate cumulative GPP, ER, NPP
df_annmag <- df_l %>%
  filter(flux != "K600.daily") %>%
  group_by(year, flux) %>%
  summarize(ann_sum = sum(value, na.rm = TRUE),
            ann_avg = sum(value, na.rm = TRUE))

# Plot mean fluxes by year
(df_l %>%
  filter(flux != "K600.daily") %>%
  ggplot(aes(x = year,
             y = value,
             color = flux)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  ylab(expression("Daily mean (g"~O[2]~d^{-1}~m^{-2}*")")) +
  xlab("") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1994, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/Daily_mean_metab_new_1993_1996.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot mean fluxes by summer
(df_l %>%
    filter(flux != "K600.daily",
           between(julian, 121, 273)) %>%
    ggplot(aes(x = year,
               y = value,
               color = flux)) +
    stat_summary(fun.y = mean, geom = "point") + 
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
    ylab(expression("Daily mean (g"~O[2]~d^{-1}~m^{-2}*")")) +
    xlab("") +
    theme_bw() +
    scale_x_continuous(breaks = seq(1994, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/Daily_mean_summer_metab_new_1993_1996.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot cumulative fluxes by year
(df_l %>%
  filter(flux != "K600.daily") %>%
  ggplot(aes(x = year,
             y = value,
             color = flux)) +
  stat_summary(fun.y = sum, geom = "point") +
  ylab(expression("Annual sum (g"~O[2]~d^{-1}~m^{-2}*")")) +
  xlab("") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1994, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/Annual_sum_metab_new_1993_1996.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot cumulative fluxes by summer
(df_l %>%
    filter(flux != "K600.daily",
           between(julian, 121, 273)) %>%
    ggplot(aes(x = year,
               y = value,
               color = flux)) +
    stat_summary(fun.y = sum, geom = "point") +
    ylab(expression("Annual sum (g"~O[2]~d^{-1}~m^{-2}*")")) +
    xlab("") +
    theme_bw() +
    scale_x_continuous(breaks = seq(1994, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/Annual_sum_summer_metab_new_1993_1996.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot k600 by year
(df_l %>%
  filter(flux == "K600.daily") %>%
  ggplot(aes(x = year,
             y = value,
             color = flux)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  ylab(expression("Daily mean"~K[600]~"("~d^{-1}*")")) +
  xlab("") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1994, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/k600_mean_metab_new_1993_1996.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Get date of 50% GPP
df_50 <- df_l %>%
  filter(type %in% c("GPP", "ER", "NPP")) %>%
  group_by(type, year) %>%
  mutate(cum = order_by(julian, cumsum(replace_na(value, 0))),
         rank = cum / max(cum)) %>%
  summarize(doy_50 = nth(julian, which.min(abs(rank -
                                                 0.5))))

# Plot gpp vs er
df %>%
  ggplot() +
  geom_point(aes(x = GPP,
                 y = ER))

df %>%
  ggplot() +
  geom_point(aes(x = K600.daily,
                 y = ER))


# Interactive dygraphs
# First need to get data in correct format
bv <- df %>%
  select(GPP) %>%
  zoo::zoo(order.by = df$date)
dygraph(bv, main = "Loire à Belleville Amont") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "GPP", independentTicks = TRUE) %>%
  # dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  dySeries("GPP", axis=('y'))
# dySeries("SC", axis=('y2'))

# Clean and gather data for Lorenz curve
df_cume <- df %>%
  select(date, GPP, ER, NPP) %>%
  mutate(ER = ifelse(ER >=0 | is.na(ER), 0, ER),
         GPP = ifelse(GPP < 0 | is.na(GPP), 0, GPP),
         NPP = GPP + ER,
         year = year(date)) %>%
  gather(flux, value, -date, -year) %>%
  group_by(year, flux) %>%
  nest()

# Function to get Lorenz curve data
cume_fun <- function(data){
  data %>%
    arrange(abs(value)) %>%
    mutate(
      x = as.numeric(date) * abs(value),
      t_rel = cumsum(as.numeric(date)) / 
        sum(as.numeric(date)),
      flux_rel = cumsum(x) / sum(x, na.rm = TRUE),
      cume_flux = cumsum(abs(value))
    )
}

# Apply function to data
df_cume <- df_cume %>%
  transmute(flux, year,
            beta = map(data, 
                       cume_fun)) %>%
  unnest()

# Gini coeff. function
gini_fun <- function(data) 
{
  n = length(data$value)
  x = sort(abs(data$value))
  G = sum(x * 1L:n)
  G = 2 * G/sum(x) - (n + 1L)
  round(G / n, digits = 2)
}

# Gini coefficient dataframe with graph properties
gini_data <- df_l %>%
  transmute(year, flux,
            gini = map(data, 
                       gini_fun)) %>%
  unnest() %>%
  # mutate(x = 0.125,
  #        y = ifelse(flow_type == "in", 0.875, 0.75)) %>%
  na.omit()

# Plot Lorenz Curves
p2 <- ggplot(data = df_cume) + 
  geom_line(aes(x = t_rel,
                y = flux_rel,
                colour = as.factor(year))) + 
  theme_bw() + 
  facet_wrap(~flux) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative fraction of time") +
  ylab("Cumulative fraction of flux magnitude") +
  theme(legend.position = "bottom",
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black"),
        legend.title = element_text(face = "bold")) #+ 
# scale_colour_manual(name = "Flow Direction",
#                     breaks = c("in",
#                                "out"),
#                     labels = c("inflow",
#                                "outflow"),
#                     values = c("#56B4E9",
#                                "#E69F00"),
#                     guide = guide_legend(ncol = 1)) +
# scale_alpha_manual(name = "Breakpoint",
#                    breaks = c(0, 1),
#                    labels = c("below",
#                               "above"),
#                    values = c(0.01,
#                               0.3),
#                    guide = guide_legend(ncol = 1)) + 
# scale_size_continuous(name = expression(bold("Cumulative Flow ("*m^3*")")),
#                       guide = guide_legend(ncol = 2),
# #                       labels = comma) +
# geom_text(data = subset(gini_data),
#           aes(x = 0.75,
#               y = 0.75,
#               label = paste0("G = ", gini),
#               colour = flux),
#           show.legend = FALSE)

p2

# Save plot
ggsave(plot = p2, 
       paste0("lorenz_", wyp, "_magnitude.tiff"),
       device = "tiff",
       width = 8,
       height = 6,
       units = "in")


# Daily metabolism predictions
# Dataframe of all metabolism estimates
df_DO <- results %>%
  map(predict_DO) %>%
  map_df(bind_rows)

bv_do <- df_DO %>%
  select(DO.obs, DO.mod) %>%
  zoo::zoo(order.by = df_DO$solar.time)

dygraph(bv_do, main = "Loire à Dampierre") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  # dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  dySeries("DO.mod", axis=('y')) %>%
  dySeries("DO.obs", axis=('y2'))