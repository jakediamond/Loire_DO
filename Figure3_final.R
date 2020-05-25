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
theme_set(theme_bw(base_size=8)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load raw metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER)

# Load metabolism and daily DO data with gap fills
df <- read_excel("Data/all_corrected_rawdata_models for detrend_residuals.xlsx", sheet = 8) %>%
  dplyr::select(-Q, -LogQ, -IS, -med_temp, -Chla,
                amp = Amplitude)
df <- df[-9486,]

# Load metabolism and daily DO data with gap fills, detrended with discharge/radiation
df_dt <- read_excel("Data/all_corrected_rawdata_models for detrend_residuals.xlsx", sheet = 7) %>%
  dplyr::select(date, gpp = GPP_Res, er = ER_res, nep = NEP_res, amp = DO_amplit_res,
                min = Domin_res)

# Short function for converting to time series format
ts_conv <- function(data){
  data = data %>%
    arrange(date) %>%
    na.trim() %>%
    as.data.frame(.) %>%
    na_kalman(.) %>%
    xts(x = .[, "value"],
        order.by = .[, "date"])
}

# Convert do data to time series format for summer and calculate ew on raw data
df_ew <- df %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, ~generic_ews(.x, winsize = 16)))

# Calculate early warnings on detrended data
df_ew_gauss <- df %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, ~generic_ews(.x, winsize = 16,
                                              detrending = "gaussian",
                                              bandwidth = 0.9)))
# First diff
df_ew_diff <- df %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = map(data, ts_conv),
         ew = map(ts_dat, ~generic_ews(.x, winsize = 16,
                                              detrending = "first-diff")))

# Convert do data to time series format for summer and calculate ew on detrended data
df_ew_dt <- df_dt %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = map(data, ts_conv),
         ew = map(ts_dat, ~generic_ews(.x, winsize = 16)))

# Calculate early warnings on detrended data
df_ew_gauss <- df %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, ~generic_ews(.x, winsize = 16,
                                              detrending = "gaussian",
                                              bandwidth = 0.9)))
# First diff
df_ew_diff <- df %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = map(data, ts_conv),
         ew = map(ts_dat, ~generic_ews(.x, winsize = 16,
                                       detrending = "first-diff")))


# Plot these
plot(pluck(df_ew_gauss,4,6,2))


# Sensitivity analysis of early warnings up to first break
df_ew_sens_brk1 <- df %>%
  pivot_longer(cols = c(-date), names_to = "key") %>%
  filter(year(date) < 2005) %>%
  group_by(key) %>%
  nest() %>%
  ungroup() %>%
  mutate(ts_dat = map(data, ts_conv))


x = pluck(df_ew_sens_brk1, 3, 6)
sens_brk1_GPP <- sensitivity_ews(x, indicator = "ar1",
                winsizerange = c(10, 50),
                incrwinsize = 20,
                bandwidthrange = c(2, 8),
                incrbandwidth = 2)
sens_brk1_ER <- sensitivity_ews(x, indicator = "ar1",
                                 winsizerange = c(10, 50),
                                 incrwinsize = 20,
                                 bandwidthrange = c(2, 8),
                                 incrbandwidth = 2)
sens_brk1_NEP <- sensitivity_ews(x, indicator = "ar1",
                                winsizerange = c(10, 50),
                                incrwinsize = 20,
                                bandwidthrange = c(2, 8),
                                incrbandwidth = 2)
sens_brk1_max_DO <- sensitivity_ews(x, indicator = "ar1",
                                 winsizerange = c(10, 50),
                                 incrwinsize = 20,
                                 bandwidthrange = c(2, 8),
                                 incrbandwidth = 2)
sens_brk1_min_DO <- sensitivity_ews(x, indicator = "ar1",
                                    winsizerange = c(10, 50),
                                    incrwinsize = 20,
                                    bandwidthrange = c(2, 8),
                                    incrbandwidth = 2)
sens_brk1_amp <- sensitivity_ews(x, indicator = "ar1",
                                    winsizerange = c(10, 50),
                                    incrwinsize = 20,
                                    bandwidthrange = c(2, 8),
                                    incrbandwidth = 2)

plot(pluck(df_ew_gauss, 4, 1, 2))
plot(sens_brk1_GPP)
saveRDS(df_ew_sens, "Data/Loire_DO/ew_sensitivity_results_brk1")

# BDS test for nonlinearity
df_ew_bds <- df %>%
  pivot_longer(cols = c(-date), names_to = "key") %>%
  group_by(key) %>%
  nest() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         bds = future_map(ts_dat, ~bdstest_ews(bind_cols(date = index(.),
                                                         value = .),
                                               ARMAoptim = FALSE,
                                               ARMAorder = c(1,0))))
saveRDS(df_ew_bds, "Data/Loire_DO/BDS_results")
# function to get time indices related to datetimes
index_fun <- function(ts_data){
  ts_data = tibble(date = index(ts_data),
                   timeindex = row_number(date),
                   value = as.numeric(ts_data))
}

# Get a dataframe of timeindices for metabolism data
df_ind <- df_ew %>%
  transmute(pdat = future_map(ts_dat, index_fun)) %>%
  unnest(cols = c(pdat)) %>%
  rename(key = name)

# Plot data for early warning signals
ew_plot_data <- df_ew %>%
  dplyr::select(-data, -ts_dat, key = name) %>%
  unnest(cols = c(ew)) %>%
  right_join(df_ind,
             by = c("timeindex",
                    "key")) %>%
  pivot_longer(cols = -c(key, timeindex, date)) %>%
  mutate(name = case_when(name == "value" ~ key, 
                          TRUE ~ name),
         value = case_when(key == "NEP" & name == "sk" ~-value,
                           TRUE ~ value))

# Plot data for early warning signals of detrended data
ew_dt_plot_data <- df_ew_gauss %>%
  dplyr::select(-data, -ts_dat, key = name) %>%
  unnest(cols = c(ew)) %>%
  right_join(df_ind,
             by = c("timeindex",
                    "key")) %>%
  pivot_longer(cols = -c(key, timeindex, date)) %>%
  mutate(name = case_when(name == "value" ~ key, 
                          TRUE ~ name))

# rename for plotting
plotnames <- tibble(name = c("ar1", "acf1", "densratio", "kurt", "cv",
                             "returnrate", "sd", "sk",
                             "NEP"),
                    plotname = c("ar(1)", "acf(1)", "density~ratio", "kurtosis", "cv",
                                 "return~rate", "standard~deviation", "skewness",
                                 "NEP~(g~O[2]~m^{-2}~d^{-1})"))
left_join(ew_plot_data, plotnames, by = "name") -> ew_plot_data
left_join(ew_dt_plot_data, plotnames, by = "name") -> ew_dt_plot_data
ew_dt_plot_data$plotname <- factor(ew_dt_plot_data$plotname,
                            levels = c("NEP~(g~O[2]~m^{-2}~d^{-1})",
                                       "ar(1)",
                                       "standard~deviation",
                                       "skewness"))
levels(ew_dt_plot_data$plotname) <- c("NEP~(g~O[2]~m^{-2}~d^{-1})",
                                  "ar(1)",
                                  "standard~deviation",
                                  "skewness")

# Also want to show detrended data
dt_data <- ew_plot_data %>%
  filter(name %in% c("GPP", "ER", "NEP", "do_min", "do_max", "Amplitude")) %>%
  group_by(key, name) %>%
  nest() %>%
  transmute(dt = future_map(data, ~ ksmooth(.$timeindex, .$value, kernel = "normal", 
                                            bandwidth = 90, #seasonal smoother
                                            range.x = range(.$timeindex), 
                                            x.points = .$timeindex)))

ew_plot_data2 <- dt_data %>%
  unnest_wider(dt) %>%
  rename(trend = y, timeindex = x) %>%
  unnest() %>%
  inner_join(ew_plot_data) %>%
  mutate(dt = trend - value)

# Want to calculate rolling statistics of early warnings
# First create the rolling function for expanding window of regression
roll_reg_fun <- function(data, n = nrow(data)) {
  # rollapplyr(1:n, 1:n, function(ix) lm(value ~ timeindex, data, subset = ix))
    rollapplyr(1:n, 1:n, function(ix) cor(x = data[ix, "timeindex"],
                                          y = data[ix, "value"],
                                          method = "kendall"))
}

df_cor <- all_plot_data %>%
  filter(!(name %in% c("returnrate", "acf1", 
                       "sd", "densratio"))) %>%
  group_by(flux, name) %>%
  nest() %>%
  mutate(roll_reg = future_map(data, roll_reg_fun))

# saveRDS(df_cor, "Data/correlations_earlywarnings")
x <- filter(all_plot_data, flux == "chla", plotname == "cv")
x <- x[1:1000,]
rollapplyr(1:nrow(x), nrow(x), 
           FUN = function(z) cor(x = x[z,"timeindex"], y =x[z, "value"],
                                 method = "kendall"),
           partial = TRUE)

# Data for plotting
df_cor_p <- unnest(df_cor, cols = c(data, roll_reg)) %>%
  right_join(tibble(#flux = c("chla", "GPP", "ER", "NEP"),
                    date = c(ymd("2004-05-01"),
                             ymd("2011-05-01"))))



x <- unnest(df_cor, cols = c(data, roll_reg))
  mutate(tidy = future_map(roll_reg, broom::tidy)) %>%
  unnest(tidy)

ggplot(data = x,
       aes(x = timeindex,
           y = roll_reg,
           color = flux)) +
  geom_line()+
  facet_wrap(~plotname)
t = ew_dt_plot_data %>%
  ungroup() %>%
  group_by(name, plotname) %>%
  mutate_at(vars(value), rescale)

# Plot the data
p_ew <- ggplot() + 
  geom_line(data = filter(ew_plot_data,
                          key == "NEP",
                          name %in% c("ar1", "sk", "sd", "NEP")),
            aes(x = date, y = value)) +
  geom_line(data = filter(ew_plot_data2,
                          key == "NEP",
                          name %in% c("NEP")),
            aes(x = date, y = trend), color = "red") +
  scale_x_datetime(date_breaks = "3 years",
               limits = c(ymd_h("1993-01-01 00"), ymd_h("2019-01-01 00")),
               date_labels = "%Y") +
  annotate(geom = "rect",
           xmin = ymd_h("2013-09-01 00"),
           xmax = ymd_h("2014-01-01 00"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
    annotate(geom = "rect",
             xmin = ymd_h("2003-07-01 00"),
             xmax = ymd_h("2004-10-01 00"),
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


p_ew_dt <- ggplot() + 
  geom_line(data = filter(ew_dt_plot_data,
                          key == "NEP",
                          # plotname != "NEP~(g~O[2]~m^{-2}~d^{-1})",
                          name %in% c("ar1", "sk", "sd")),
            aes(x = date, y = value)) +
  geom_line(data = filter(ew_plot_data2,
                          key == "NEP",
                          # plotname != "NEP~(g~O[2]~m^{-2}~d^{-1})",
                          name %in% c("NEP")),
            aes(x = date, y = dt), color = "black") +
  scale_x_datetime(date_breaks = "3 years",
                   limits = c(ymd_h("1993-01-01 00"), ymd_h("2019-01-01 00")),
                   date_labels = "%Y") +
  annotate(geom = "rect",
           xmin = ymd_h("2013-09-01 00"),
           xmax = ymd_h("2014-01-01 00"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
  annotate(geom = "rect",
           xmin = ymd_h("2003-07-01 00"),
           xmax = ymd_h("2004-10-01 00"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "#008B45FF") +
  theme(legend.position = c(0.12, 0.6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_wrap(~plotname, scales = "free_y",  ncol = 1, labeller = label_parsed)
p_ew_dt

(cowplot::plot_grid(p_ew, p_ew_dt, nrow = 1, labels = c('a', 'b'),
                    label_x = 0.1, label_y = 0.95, label_size = 8)) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure3_final.png",
         device = "png",
         dpi = 300,
         width = 183,
         height = 100,
         units = "mm")
