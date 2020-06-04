# 
# Purpose: To do early warning analysis of Middle Loire data
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
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

# Load raw metabolism data up to the chlorophyll change point
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  select(-NPP, -K600.daily) %>%
  filter(year(date) < 2006) %>%
  na.trim() %>%
  na_kalman() %>% # fill any small gaps
  pivot_longer(-date) %>%
  group_by(name) %>%
  nest() %>%
  mutate(ts = map(data, ~ts(data = .x$value)))

# Load DO data up to the chlorophyll change point
df_do <- read_excel("Data/all_corrected_rawdata_models for detrend_residuals.xlsx", sheet = 8) %>%
  dplyr::select(-Q, -LogQ, -IS, -med_temp, -Chla, -GPP, -ER, -NEP,
                amp = Amplitude) %>%
  slice(-9486) %>%
  pivot_longer(-date) %>%
  filter(year(date) < 2006) %>%
  group_by(name) %>%
  nest() %>%
  mutate(ts = map(data, ~ts(data = .x$value)))

# Load hydrochemistry data up to the chlorophyll change point
df_sol <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  filter(solute %in% c("PO4", "CHLA", "SPM")) %>%
  group_by(solute, year, month) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  filter(between(year, 1980, 2005)) %>%
  filter(!(year < 1986 & solute == "SPM")) %>% # huge gap between 1980 and 1986
  filter(!(year < 1981 & solute == "PO4")) %>% # data start in earnest in 1982
  ungroup() %>%
  group_by(solute) %>%
  na_kalman() %>% # fill any small gaps
  nest() %>%
  mutate(ts = map(data, ~ts(data = .x$value))) %>%
  rename(name = solute)

# Combine all data
df <- bind_rows(df_sol, df_met, df_do)

# Create the dataframe for testing sensitivity of different detrends/indicators
sens_met_df <- df_met %>%
  ungroup() %>%
  mutate(dt = c("no", "gaussian", "first-diff"),
         indicator =c("ar1", "sd", "sk")) %>%
  expand(name, dt, indicator) %>%
  left_join(df_met, by = "name")

# Sensitivity function
s_fun <- function(ts, indicator, dt){
  sensitivity_ews(timeseries = ts, indicator = indicator, 
                  winsizerange = c(10,50), 
                  incrwinsize = 200, 
                  detrending = dt, 
                  bandwidthrange = c(10,10),
                  incrbandwidth = 0)
}

# Do sensitivity analysis
sens_met <- sens_met_df %>%
  filter(!(name == "NEP" & (dt %in% c("gaussian", "first-diff")))) %>%
  mutate(s = pmap(list(ts, indicator, dt), s_fun))
saveRDS(sens_met, "Data/Loire_DO/met_ew_sensitivity")

sens_met_dat <- sens_met %>%
  select(name, indicator, dt, s) %>%
  filter(dt != "gaussian") %>%
  mutate(s = map(s, rownames_to_column, var = "timeindex")) %>%
  unnest(s) %>%
  bind_rows(sens_met %>%
              select(name, indicator, dt, s) %>%
              filter(dt == "gaussian") %>%
              unnest(s) %>%
              pivot_longer(cols = -c(1:3), names_to = "timeindex", values_to = "Ktauestind")) %>%
  drop_na() %>%
  mutate(timeindex = as.numeric(timeindex)) %>%
  left_join(select(df_met, name, data) %>%
              group_by(name) %>%
              transmute(length = map(data, nrow)) %>%
              unnest(cols = length)) %>%
  mutate(size = timeindex / 365)

(ggplot(data = sens_met_dat,
       aes(x = size,
           y = Ktauestind,
           color = dt,
           group = dt)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(-1,1)) +
  theme_bw(base_size = 8) +
  facet_grid(rows = vars(indicator), cols = vars(name)) + 
  scale_color_brewer(name = "detrending", type = "qual") + 
  ylab(expression(Kendall~tau)) +
  xlab("rolling window size (years)")) %>%
  ggsave(filename = "Figures/supplementary/met_ew_sensitivty_tau.png",
         device = "png",
         width = 150,
         height = 150,
         dpi = 300,
         units = "mm")


# Early warnings graphs
ews_df_met <- df_met %>%
  ungroup() %>%
  mutate(dt = c("no", "gaussian", "first-diff")) %>%
  expand(name, dt) %>%
  left_join(df, by = "name")

# early warnings
ews_met <- ews_df_met %>%
  mutate(ew = map2(ts, dt, ~generic_ews(.x, winsize = 33, detrending = .y, bandwidth = 2)))
saveRDS(ews_met, "Data/Loire_DO/met_ew_sensitivity_ts")
# Data for plotting
ew_met_dat <- ews_met %>%
  select(name, data, dt, ew) %>%
  mutate(ind = map(ew, pluck, 1), 
         dat = map2(data, ind, slice),
         ew = map2(ew, dat, bind_cols)) %>%
  select(name, dt, ew) %>%
  unnest(ew) %>%
  filter(!(name == "NEP" & (dt %in% c("gaussian", "first-diff")))) %>%
  mutate(ar1 = if_else(dt == "first-diff", ar1 + 0.5, ar1)) %>%
  # mutate(date = ymd(paste(year,month,"01"))) %>%
  select(type = name, date, dt, ar1, sd, sk) %>%
  pivot_longer(cols = -c(type, date, dt))
x = pluck(df_met, 3,3)
plot(x)
generic_ews(x, winsize = 25)
(ggplot(data = ew_met_dat,
       aes(x = date,
           y = value,
           color = dt,
           group = dt)) +
  geom_line() +
  facet_grid(cols = vars(type), rows =vars(name), scales = "free_y",
             switch = "y") +
  theme_bw(base_size = 8) +
  scale_color_brewer(name = "detrending", type = "qual") + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title = element_blank())) %>%
  ggsave(filename = "Figures/supplementary/met_ew_sensitivty_ts.png",
         device = "png",
         width = 150,
         height = 150,
         dpi = 300,
         units = "mm")



