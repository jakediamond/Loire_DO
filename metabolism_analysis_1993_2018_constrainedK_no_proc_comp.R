# 
# Purpose: To analyze metabolism 1993-2018 data for Loire River
# Author: Jake Diamond
# Date: September 19, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(streamMetabolizer)
library(tidyverse)
library(lubridate)
library(dygraphs)

# Load metab data
df <- readRDS("Data/Loire_DO/metab_results_1993_2018_constrainedK")
df_mle <- readRDS("Data/Loire_DO/metab_mle") %>%
  pluck("metab_daily") %>%
  rename_at(vars(-date), function(x) paste0(x,"_mle"))
df_unconstrain <- readRDS("Data/Loire_DO/metab_results_1994_2018.rds") %>%
  rename_at(vars(-date), function(x) paste0(x, "_uncon"))
# Load cleaned DO data
df_do <- readRDS("Data/all_DO_cleaned")



mm_no_proc <- readRDS("Data/Loire_DO/metab_constrainedK_noproc_1997_2000")
mm_no_proc2 <- readRDS("Data/Loire_DO/metab_constrainedK_noproc_1993_1995")
# Dataframe of all 1995-2000 metabolism estimates
df_no_proc <- mm_no_proc[2, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_no_proc[2, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  rename_at(vars(-date), function(x) paste0(x, "_nop"))

df_no_proc <- mm_no_proc2[1, ] %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(mm_no_proc2[1, ] %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  rename_at(vars(-date), function(x) paste0(x, "_nop")) %>%
  bind_rows(df_no_proc)



# Summarize daily amplitude for DO data
df_amp <- df_do %>%
  filter(site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max = max(filtered, na.rm = TRUE),
            min = min(filtered, na.rm = TRUE),
            amp = max - min)
df_amp_l <- df_amp %>%
  mutate(year = year(date)) %>%
  ungroup() %>%
  gather(var, val, max, min, amp)
ggplot(data = df_amp_l,
       aes(x = val)) +
  geom_density(aes(fill = as.factor(year)),
               alpha = 0.2) +
  facet_wrap(~var)

# Join data
df_all <- left_join(df, df_amp) %>%
  left_join(df_mle) %>%
  left_join(df_unconstrain) %>%
  left_join(df_no_proc)

(ggplot(data = df_all,
       aes(x = GPP,
           y = GPP_nop,
           color = as.factor(year(date)))) +
  geom_point() + facet_wrap(~as.factor(year(date))) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  scale_y_continuous(limits = c(0,25)) +
  scale_x_continuous(limits = c(0,25))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/GPPmle_vs_GPP_con.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

(ggplot(data = df_all,
       aes(x = amp,
           y = GPP_nop,
           color = as.factor(year(date)))) +
  geom_point() + facet_wrap(~as.factor(year(date))) +
  geom_vline(aes(xintercept = 10))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/GPP_vs_amplitude.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Get data in long format
df_l <- df %>%
  select(date, GPP, ER, NPP, K600.daily) %>%
  mutate(ER = ifelse(ER >=0, 0, ER),
         GPP = ifelse(GPP < 0, 0, GPP),
         NPP = GPP + ER,
         year = year(date),
         julian = yday(date)) %>%
  gather(flux, value, -date, -year, -julian)

# Plot time series of GPP
(df %>%
  mutate(GPP = ifelse(GPP < 0, 0, GPP)) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = GPP)) +
  # geom_ribbon(aes(ymin = GPP.lower,
  #                 ymax = GPP.upper),
  #             alpha = 0.5,
  #             fill = "blue") +
  theme_classic()) %>%
  ggsave(plot = .,
         filename = "Figures/To share/GPP_timeseries.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot all time series
(df_l %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value,
                color = flux)) +
  facet_wrap(~flux, scales = "free") +
  theme_classic()) %>%
  ggsave(plot = .,
         filename = "Figures/To share/all_time_series.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot cumulative years
(df_l %>%
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
  theme(legend.position = "right") +
  ylab(expression("Cumulative value (g"~O[2]~d^{-1}~m^{-2}*")")) +
  xlab("Julian Day")) %>%
  ggsave(plot = .,
         filename = "Figures/To share/cumulative_years.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

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
  scale_x_continuous(breaks = seq(1993, 2018, 2))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/Daily_mean_metab_constrainedK.tiff",
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
         filename = "Figures/To share/Daily_mean_summer_metab_constrainedK.tiff",
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
         filename = "Figures/Annual_sum_metab_constrainedK.tiff",
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
         filename = "Figures/Annual_sum_summer_metab_constrainedK.tiff",
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
         filename = "Figures/k600_mean_metab_constrainedK.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Get date of 50% GPP
df_50 <- df_l %>%
  filter(flux %in% c("GPP", "ER", "NPP")) %>%
  group_by(flux, year) %>%
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
df_l2 <- df_cume %>%
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
gini_data <- df_cume %>%
  transmute(year, flux,
            gini = map(data, 
                       gini_fun)) %>%
  unnest() %>%
  # mutate(x = 0.125,
  #        y = ifelse(flow_type == "in", 0.875, 0.75)) %>%
  na.omit()

# Plot Lorenz Curves
p2 <- ggplot(data = df_l2) + 
  geom_line(aes(x = t_rel,
                y = flux_rel,
                colour = as.factor(year))) + 
  theme_bw() + 
  facet_wrap(~flux) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative fraction of time") +
  ylab("Cumulative fraction of flux magnitude") +
  scale_color_viridis_d() +
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
       filename = "Figures/To share/Lorenz.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       units = "in")

(ggplot(data = gini_data,
        aes(x = year,
            y = gini,
            color = year)) +
    geom_point() +
    geom_line() +
    facet_wrap(~flux) + 
    scale_color_viridis_c())


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