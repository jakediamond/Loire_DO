# 
# Purpose: To analyze metabolism 1993-2018 data for Loire River
# Author: Jake Diamond
# Date: September 19, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("D:/jake.diamond/Loire_DO")

# Load libraries
library(streamMetabolizer)
library(tidyverse)
library(lubridate)
library(dygraphs)

# Load metab data
df <- readRDS("Data/Loire_DO/metab_veryconstrainedK")
# df_mle <- readRDS("Data/Loire_DO/metab_mle") %>%
#   pluck("metab_daily") %>%
#   rename_at(vars(-date), function(x) paste0(x,"_mle"))
df_nopool <- readRDS("Data/Loire_DO/metabolism_results_all_years_constrainedK_no_pool")
df_unconstrain <- readRDS("Data/Loire_DO/metab_unconstrainedK")
df_gpp_err_obs <- readRDS("Data/Loire_DO/metab_veryconstrainedKerr_proc_gpp_true_def_obs")
df_gpp_err <- readRDS("Data/Loire_DO/metab_veryconstrainedK_err_proc_gpp_true")
df_go <- df_gpp_err_obs %>%
  ungroup() %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(df_gpp_err_obs %>%
              ungroup() %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  select(date, GPP, ER, NPP, K600.daily) %>%
  rename_at(vars(-date), function(x) paste0(x, "_go"))
z<-predict_DO(pluck(df_gpp_err_obs, 2, 1))
plot(z$solar.time[2000:2100], z$DO.obs[2000:2100])
points(z$solar.time[2000:2100], z$DO.mod[2000:2100], col = "blue")
points(df_proc$solar.time[1500:1600], df_proc$err_proc_iid_mean[1500:1600] +12, col = 'red')
plot(df_proc$solar.time[1500:1600], df_proc$err_proc_iid_mean[1500:1600], col = 'red')

df_obs <- readRDS("F:/DataLocalePerso/Jake/metab_veryconstrainedK_def_obs")
df_obs <- df_obs %>%
  ungroup() %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(df_obs %>%
              ungroup() %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  select(date, GPP, ER, NPP, K600.daily) %>%
  rename_at(vars(-date), function(x) paste0(x, "_obs"))
# Load cleaned DO data
df_do <- readRDS("Data/all_DO_cleaned")

df_mod <- df_gpp_err %>%
  ungroup() %>%
  transmute(proc_err = map(mm, predict_DO)) %>%
  # transmute(mod = map(proc_err, ~pluck(., 5))) %>%
  unnest()

plot(df_mod$DO.obs[4000:5000], df_mod$DO.mod[4000:5000])
abline(0,1)

df_proc <- df_gpp_err_obs %>%
df_proc <- df %>%
  ungroup() %>%
  transmute(proc_err = map(mm, get_fit)) %>%
  transmute(
    proc_err_mean = map(proc_err, ~pluck(., 2))) %>%
  unnest() %>%
  select(date, )
ggplot(data = df_proc, aes(x = solar.time, y = err_proc_iid_mean)) + geom_point()
ggplot(data = df_proc, aes(x = hour(solar.time), y = err_proc_iid_mean)) + 
  stat_summary(fun.data = "mean_cl_boot", na.rm = T)
x <- filter(df_proc, err_proc_iid_mean > 10)
plot(df_proc$solar.time[500:600], df_proc$err_proc_iid_mean[500:600])
plot(df_proc$solar.time[500:600], df_proc$err_proc_GPP_mean[500:600])
plot(x$solar.time[500:505], x$err_proc_iid_mean[500:505])
plot(x$solar.time[500:505], x$err_proc_GPP_mean[500:505])


df_proc2 <- df_obs %>%
  ungroup() %>%
  transmute(proc_err = map(mm, get_fit)) %>%
  transmute(
    proc_err_mean = map(proc_err, ~pluck(., 2))) %>%
  unnest()

df_proc_gpp <- df_gpp_err %>%
  ungroup() %>%
  transmute(proc_err = map(mm, get_fit)) %>%
  transmute(
    proc_err_mean = map(proc_err, ~pluck(., 2))) %>%
  unnest()

df_comp <- left_join(df_proc, df_proc2, by = c("date", "solar.time"))
df_proc_nopool <- df_nopool %>%
  ungroup() %>%
  transmute(proc_err = map(mm, get_fit)) %>%
  transmute(
    proc_err_mean = map(proc_err, ~pluck(., 2))) %>%
  unnest()

df_comp <- left_join(df_proc, df_proc_nopool, by = c("date", "solar.time"))
df_comp <- left_join(df_comp, df_proc_gpp)
plot(df_comp$solar.time[4000:5000], df_comp$err_proc_iid_mean[4000:5000] + df_comp$err_proc_GPP_mean[4000:5000] )
plot(df_comp$err_proc_iid_mean.y, df_comp$err_proc_iid_mean+ df_comp$err_proc_GPP_mean)
plot(df_proc2$err_proc_iid_mean[1:5000], df_proc$err_proc_iid_mean[1:5000])
plot(df_proc$solar.time[4000:5000], df_proc2$err_proc_iid_mean[4000:5000])
plot(df_proc2$err_proc_iid_mean[1:5000], df_proc$err_proc_iid_mean[1:5000])
# Get data in dataframe format
df_all <- df %>%
  ungroup() %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  mutate(NPP = GPP + ER) %>%
  left_join(df %>%
              ungroup() %>%
              mutate(parms = map(mm, get_params)) %>%
              unnest(parms) %>%
              select(date, K600.daily)) %>%
  rename_at(vars(-date), function(x) paste0(x, "_con")) %>%
  left_join(df_unconstrain %>%
              ungroup() %>%
              mutate(met = map(mm, predict_metab)) %>%
              unnest(met) %>%
              mutate(NPP = GPP + ER) %>%
              left_join(df_unconstrain %>%
                          ungroup() %>%
                          mutate(parms = map(mm, get_params)) %>%
                          unnest(parms) %>%
                          select(date, K600.daily)) %>%
              rename_at(vars(-date), function(x) paste0(x, "_uncon")))
  left_join(df_nopool %>%
              rename_at(vars(-date), function(x) paste0(x, "_nopool")))
  # left_join(df_unconstrain %>%
  #             ungroup() %>%
  #             mutate(met = map(mm, predict_metab)) %>%
  #             unnest(met) %>%
  #             mutate(NPP = GPP + ER) %>%
  #             left_join(df_unconstrain %>%
  #                         ungroup() %>%
  #                         mutate(parms = map(mm, get_params)) %>%
  #                         unnest(parms) %>%
  #                         select(date, K600.daily)) %>%
  #             rename_at(vars(-date), function(x) paste0(x, "_uncon")))

# Summarize daily amplitude for DO data
df_amp <- df_do %>%
  filter(site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max = max(filtered, na.rm = TRUE),
            min = min(filtered, na.rm = TRUE),
            amp = max - min)
# Get in long format
df_amp_l <- df_amp %>%
  mutate(year = year(date)) %>%
  ungroup() %>%
  gather(var, val, max, min, amp)
# Quick plot
ggplot(data = df_amp_l,
       aes(x = val)) +
  geom_density(aes(fill = as.factor(year)),
               alpha = 0.2) +
  facet_wrap(~var)

# Join data
df_all <- df_all %>%
  ungroup() %>%
  select(date, GPP_nop, ER_nop, NPP_nop, K600.daily_nop, GPP_uncon, ER_uncon, NPP_uncon, K600.daily_uncon) 
df_all2 <- df_all %>%
  filter(!(is.na(GPP_uncon))) %>%
  select(date, ends_with("uncon")) %>%
  left_join(filter(df_all, !(is.na(GPP_nop))) %>%
              select(date, ends_with("nop")), by = "date")
df_all_3 <- left_join(df_all2, df_go)
df_all_4 <- left_join(df_all_3, df_obs)

df_all <- left_join(df, df_amp)
df

# Process error time series
(ggplot(data = df,
        aes(x = date,
            y = mean_proc_err)) +
    geom_point()) %>%
  ggsave(plot = .,
         filename = "Figures/To share/mean_proc_err.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

ggplot(data = df_all_4,
       aes(x = GPP_nop,
           y = GPP_go,
ggplot(data = df_all,
       aes(x = K600.daily_con,
           y = K600.daily_nopool,
           color = as.factor(month(date)))) +
  geom_point() + facet_wrap(~as.factor(year(date))) +
    scale_color_viridis_d() +
  geom_abline(aes(slope = 1, intercept = 0))


ggplot(data = df_all) +
geom_point(aes(x = date,
           y = K600.daily_nopool),
           color = "red") +
  geom_point(aes(x = date,y = K600.daily_con),
             color = "blue") + facet_wrap(~as.factor(year(date)),
                                          scales = "free") +
  scale_color_viridis_d()

  ggsave(plot = .,
         filename = "Figures/To share/GPPuncons_vs_GPPcons.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Autocorrelation
  ggplot(data = df_do,
         aes(x = DO_use,
             y = lead(DO_use),
             color = as.factor(month(datetime)))) +
    geom_point() + facet_wrap(~as.factor(year(datetime))) +
    scale_color_viridis_d() +
    geom_abline(aes(slope = 1, intercept = 0))
  
-log(density(na.omit(filter(df_do, site == "dampierre")$DO_use)))
  
  ggplot(data = filter(df_do, site == "dampierre")) +
    geom_histogram(aes(x = DO_use)) +
    facet_wrap(~as.factor(year(datetime))) +
    scale_color_viridis_d() +
    geom_abline(aes(slope = 1, intercept = 0))
  
(ggplot(data = df_all,
       aes(x = amp,
           y = GPP_nop,
           color = as.factor(year(date)))) +
  geom_point() + facet_wrap(~as.factor(year(date))) +
  geom_vline(aes(xintercept = 10))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/GPP_nop_vs_amplitude.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Get data in long format
df_l <- df %>%
df_l <- df_nopool %>%
  select(date, GPP, ER, NPP, K600.daily) %>%
  mutate(ER = ifelse(ER >=0, 0, ER),
         GPP = ifelse(GPP < 0, 0, GPP),
         NPP = GPP + ER,
         year = year(date),
         julian = yday(date)) %>%
  gather(flux, value, -date, -year, -julian)

# Plot time series of GPP and ER for 2008
(df %>%
    mutate(GPP = ifelse(GPP < 0, 0, GPP),
           ER = ifelse(ER >=0, 0, ER),
           year = year(date)) %>%
    filter(year == 2008) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = GPP), color = "dark green") +
    geom_point(aes(y = ER), color = "brown") +
  geom_ribbon(aes(ymin = GPP.lower,
                  ymax = GPP.upper),
              alpha = 0.5,
              fill = "dark green") +
    geom_ribbon(aes(ymin = ER.lower,
                    ymax = ER.upper),
                alpha = 0.5,
                fill = "brown") +
  theme_bw() +
    scale_y_continuous(limits = c(-30, 40),
                       breaks = seq(-30, 40, 10))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/GPP_timeseries_2008_compare_matt.tiff",
         device = "tiff",
         width = 8,
         height = 6,
         units = "in")

# Plot time series of GPP and ER for 2008
(df %>%
    mutate(GPP = ifelse(GPP < 0, 0, GPP),
           ER = ifelse(ER >=0, 0, ER),
           year = year(date),
           PR = GPP / abs(ER)) %>%
    filter(year == 2008) %>%
    ggplot(aes(x = date)) +
    geom_point(aes(y = PR), color = "dark green") +
    theme_bw() +
    scale_y_continuous(limits = c(-2, 8),
                       breaks = seq(-2, 8, 1))) %>%
  ggsave(plot = .,
         filename = "Figures/To share/PR_timeseries_2008_compare_matt.tiff",
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
         filename = "Figures/To share/all_time_series_nopool.tiff",
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
         filename = "Figures/To share/cumulative_years_nopool.tiff",
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
         filename = "Figures/To share/Daily_mean_metab_constrainedK_nopool.tiff",
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
         filename = "Figures/To share/Daily_mean_summer_metab_constrainedK_nopool.tiff",
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
dygraph(bv, main = "Loire à Dampierre Amont") %>% 
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


df_l %>%
  # mutate(GPP = ifelse(GPP < 0, 0, GPP),
  #        ER = ifelse(ER >=0, 0, ER),
  #        year = year(date),
  #        PR = GPP / abs(ER)) %>%
  ggplot() +
  geom_histogram(aes(value, fill = as.factor(year)), alpha = 0.3) +
  facet_wrap(~flux, scales = "free") +
  scale_fill_viridis_d()
# df_nopool <- df_met
library(lmtest)
grangertest(diff(df_nopool$GPP[8900:9032]),diff(df_nopool$ER[8900:9032], order = 2))
grangertest(diff(df_nopool$K600.daily[8900:9032]), diff(df_nopool$GPP[8900:9032], order = 1))
plot(df_nopool$GPP[1:365])
plot(diff(df_nopool$K600.daily[1:365]))
library(vars)
dat <- df_nopool %>%
  ungroup() %>%
  mutate(ER = ifelse(ER >=0, NA, diff(ER)),
         GPP = ifelse(GPP < 0, NA, diff(GPP))) %>%
  dplyr::select(GPP:K600.daily) %>%
  drop_na()
v <- VAR(dat[1:100, ],
         type = "const",
         p = 1)
causality(v, cause = "K600.daily")
x <- dplyr::select(ungroup(df_nopool), GPP) %>%
  drop_na()

library(earlywarnings)
ch_ews(timeseries = x[1:100, ])
generic_ews(x[1:720,])
