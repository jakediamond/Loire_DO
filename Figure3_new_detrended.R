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

# Set the plotting theme
theme_set(theme_bw(base_size=8)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load middle loire water quality data
df_chla <- read_excel("Data/Chla_GPP_11 stations_Gien to Villandry_et HFCM.xlsx", sheet = 4) %>%
  select(date = dateinterp, chla = chlainterp)

# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) #%>%
  # left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  # mutate(GPPq = GPP / log(discharge.daily),
  #        ERq = ER / log(discharge.daily),
  #        NEPq = NEP / log(discharge.daily))
df_met <- read_csv2("Data/flor_data_test_granger.csv") %>%
  transmute(date = dmy(date),
         GPP = gpp,
         ER = er,
         NEP = GPP + ER) %>%
  right_join(select(df_met1, date))
# Load detrended DO data
df_do_dt <- read_excel("Data/Loire_DO/loire_do_chla_detrend.xlsx")
# Load all detrended data
df_dt <- read_excel("Data/all_corrected_rawdata_models for detrend_residuals.xlsx", sheet = 7)
do_ews_amp <- generic_ews(data, winsize = 15)
df_sens_amp <- sensitivity_ews(data, indicator = "ar1", winsizerange = c(12, 27),
                               incrwinsize = 3)

# Short function for converting to time series
ts_conv <- function(data){
  data = data %>%
    arrange(date) %>%
    # mutate(ind = row_number()) %>%
    na.trim() %>%
    #   as.data.frame(.)
    # dat_ts = data %>%
    #   na.trim(.) %>%
    as.data.frame(.) %>%
    na_kalman(.) %>%
    # na_seadec(.,find_frequency = TRUE, algorithm = "kalman") %>%
    xts(x = .[, "value"],
        order.by = .[, "date"])
}

# Convert chla data to time series format for summer
df_chla_ts <- df_chla %>%
  as.data.frame(.) %>%
  filter(between(month(date), 4, 10)) %>%
  xts(x = .[, "chla"],
      order.by = .[, "date"])

df_chla_ew <- df_chla_ts %>%
  generic_ews(., winsize = 12)

df_chla_ew <- tibble(date = index(df_chla_ts),
                 timeindex = row_number(date)) %>%
  right_join(df_chla_ew)

# Convert do data to time series format for summer and calculate ew
df_dt_ew <- df_dt %>%
  # select(1:8) %>%
  as.data.frame(.) %>%
  # filter(between(month(date), 4, 10)) %>%
  pivot_longer(cols = -date, names_to = "flux") %>%
  group_by(flux) %>%
  nest() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, generic_ews, winsize = 16,
                         logtransform = FALSE))

# Analyze metabolism data
df_ts_met <- df_met %>%
  # filter(between(month(date), 4, 10)) %>%
  # filter(year(date) != 2001,
  #        between(month(date), 4, 10)) %>%
  # select(-K600.daily, -NPP) %>%
  pivot_longer(cols = c(GPP, ER, NEP), names_to = "flux") %>%
  group_by(flux) %>%
  nest() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, generic_ews, winsize = 12,
                         logtransform = FALSE))
         # sens = future_map(ts_dat, sensitivity_ews, indicator = "ar1",
         #          winsizerange = c(12, 27),
         #          incrwinsize = 3))
         # , bds = future_map(ts_dat, ~bdstest_ews(bind_cols(date = index(.), 
         #                                                 value = .),
         #                                       ARMAoptim = FALSE,
         #                                       ARMAorder = c(1,0))))
x = pluck(df_ts_met, 4, 3)
y = pluck(df_ts_met, 3, 3)
plot(as.numeric(x))
generic_ews(x, winsize = 12,
            detrending = "gaussian")
df_ts_met_sens <- df_ts_met
# function to get time indices related to datetimes
index_fun <- function(ts_data){
  ts_data = tibble(date = index(ts_data),
                   timeindex = row_number(date),
                   value = as.numeric(ts_data))
}

# Get a dataframe of timeindices for metabolism data
df_ind <- df_dt_ew %>%
  transmute(pdat = future_map(ts_dat, index_fun)) %>%
  unnest(cols = c(pdat)) %>%
  select(-value)

# And for DO:chla data
df_ind2 <- df_do_ew %>%
  transmute(pdat = future_map(ts_dat, index_fun)) %>%
  unnest(cols = c(pdat)) %>%
  select(-value)

# Plot data for early warning signals of GPP/ER/NEP
met_plot_data <- df_ts_met %>%
  # mutate(pdat = future_map(ts_dat, index_fun)) %>%
  select(-data, -ts_dat) %>%
  unnest(cols = c(ew)) %>%
  pivot_longer(cols = -c(flux, timeindex)) %>%
  left_join(df_ind,
            by = c("timeindex",
                   "flux")) %>%
  mutate(name = case_when(flux == "NEP" & name == "sd" ~ "cv",
                           flux == "NEP" & name == "cv" ~ "sd",
                           TRUE ~ name)) #want to show SD for NEP along with CV of others

chla_do_plot_data <- df_do_ew %>%
  # mutate(pdat = future_map(ts_dat, index_fun)) %>%
  select(-data, -ts_dat) %>%
  unnest(cols = c(ew)) %>%
  pivot_longer(cols = -c(flux, timeindex)) %>%
  left_join(df_ind2,
            by = c("timeindex",
                   "flux")) %>%
  mutate(date = as.Date(date))

# rename for plotting
plotnames <- tibble(name = c("ar1", "acf1", "densratio", "kurt", "cv",
                             "returnrate", "sd", "sk"),
                    plotname = c("ar(1)", "acf(1)", "density ratio", "kurtosis", "cv",
                                 "return rate", "standard deviation", "skewness"))
left_join(met_plot_data, plotnames, by = "name") -> met_plot_data

# chla_do_plot_data <- df_do_ew %>%
#   pivot_longer(cols = -c(date, timeindex)) %>%
#   left_join(plotnames, by = "name") %>%
#   mutate(date = as.Date(date),
#          flux = "chla")
# 
# do_plot_data <- tibble(date = index(data),
#          timeindex = row_number(date)) %>%
#   right_join(do_ews_amp) %>%
#   pivot_longer(cols = -c(date, timeindex)) %>%
#   left_join(plotnames, by = "name") %>%
#   mutate(date = as.Date(date),
#          flux = "DO")

# Get all early warnings data in one place
all_plot_data <- bind_rows(chla_do_plot_data, met_plot_data)

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

# Plot the data
p_ew <- ggplot() + 
  geom_line(data = filter(all_plot_data, name == "ar1"),
                          # !(name %in% c("returnrate", "acf1", 
                          #                    "sd", "densratio"))),
            aes(x = date, y = value,
                color = flux)) +
  # geom_text(data = df_cor_p,
  #           aes(x = date,
  #               y = value*0.95,
  #               color = flux,
  #               label = round(roll_reg, 2)),
  #           show.legend = FALSE) +
    scale_x_date(date_breaks = "3 years",
                 # limits = c(ymd("2000-01-01"), ymd("2019-01-01")),
                 date_labels = "%Y") + 
  annotate(geom = "rect",
           xmin = ymd("2011-07-01"),
           xmax = ymd("2013-07-01"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
    annotate(geom = "rect",
             xmin = ymd("2004-07-01"),
             xmax = ymd("2006-07-01"),
             ymin = -Inf,
             ymax = Inf,
             alpha = 0.4,
             fill = "red") +
  scale_color_manual(name = "",
                     breaks = c("chla_res", "do_amp_res", "do_min_res", "ER", "GPP", "NEP"),
                     values = c("red", "black", "grey","light blue", "dark blue","dark green"),
                     labels = c("chl. a", "DO_amp", "DO_min", "ER", "GPP", "NEP")) +
  theme(legend.position = c(0.4, 0.6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~plotname, scales = "free_y", strip.position = "left")
p_ew

  ggsave(plot = p_ew,
         filename = "Figures/Middle_Loire/Figure3_svg.svg",
         device = "svg",
         dpi = 300,
         width = 183,
         height = 100,
         units = "mm")
