# 
# Purpose: To plot Figure 1, time series changepoints for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(ggsci)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(mcp)
library(changepoint)
library(imputeTS)
library(tidyverse)

# Set seed
set.seed(42)

# # Set the plotting theme
theme_set(theme_bw(base_size=8)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
                  plot.tag.position = c(0.1,0.96)))

# Loading data and cleaning -----------------------------------------------
# Load long term wq data
df <- readRDS("Data/Loire_DO/all_longterm_wq_data") %>%
  bind_rows(read_xlsx("Data/Loire_DO/tabPC.xlsx") %>%
              dplyr::rename(site_no = cd_site,
                            date = date_opecont,
                            TEMP_EAU = `Temp. eau`,
                            COND = `Conductiv.25°C`,
                            ChOD = DCO,
                            NKj = NKJ,
                            O2 = `O2 dissous`,
                            NH4 = `NH4+`,
                            NO3 = `NO3-`,
                            SIO = `SiO2`,
                            PTO = `P total`,
                            PO4 = `Orthophosp`,
                            PheoP = `PHEOPIG.`,
                            CHLA = `CHL.A`) %>%
              dplyr::select(-`Conductiv.20°C`, -cd_opecont) %>%
              mutate(date = as.Date(date),  
                     site_no = str_pad(site_no, 8, pad = "0"),
                     Annee = year(date),
                     Mois = month(date),
                     Jour = day(date)))

# Bit of cleaning
df <- df %>%
  distinct() %>%
  rename(DOC = COD,
         TKN = NKj,
         SiO2 = SIO,
         temp = TEMP_EAU,
         SC = COND,
         TP = PTO,
         DO = O2,
         SPM = MES,
         BOD5 = DBO5,
         year = Annee,
         month = Mois,
         day = Jour)

# Just middle Loire sites
df_mid_wq <- filter(df, site_no %in% c("04048000", #Dampierre
                                       "04050500" #Orleans
                                       )) %>%
  distinct(site_no, date, .keep_all = TRUE)

# Load long term DO data
df_do <- readRDS("Data/all_DO_cleaned") %>%
  group_by(site) %>%
  distinct(datetime, .keep_all = TRUE) %>%
  arrange(datetime) %>%
  ungroup() %>%
  filter(site == "dampierre") %>%
  left_join(readRDS("Data/all_DO_data") %>%
              dplyr::filter(var == "T",
                            site == "dampierre") %>%
              select(datetime, temp = value),
            by = "datetime") %>%
  left_join(readRDS("Data/Loire_DO/dampierre_temp_estimates"), by = "datetime") %>%
  mutate(temp = ifelse(is.na(temp), temp.water, temp)) %>%
  select(-date, -temp.water) %>%
  mutate(DO_sat = ifelse(temp <= 0,
                      0,
                      14.652 - 0.41022 * temp + 0.007991 * 
                        temp^2 - 0.000077774 * temp^3),
         DO_per = DO_use / DO_sat)

# Load corbicula data
df_cor <- read_xlsx("Data/Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2)

# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Get metabolism into long format and prepare for plotting (filter 4 bad days not caught earlier)
df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  left_join(df_q) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  pivot_longer(cols = c(-date), names_to = "key", values_to = "value") %>%
  mutate(value = case_when(between(date, ymd("2001-12-15"), ymd("2001-12-18"))~ 0,
                           TRUE ~ value)) %>%
  mutate(type_plot = dplyr::recode(key,
                            `ER` = "ER~(g~O[2]~m^{-2}~d^{-1})",
                            `GPP` = "GPP~(g~O[2]~m^{-2}~d^{-1})",
                            `NEP` = "NEP~(g~O[2]~m^{-2}~d^{-1})",
                            `K600.daily` = "k[600]~(d^{-1})",
                            `discharge.daily` = "Discharge~(m^3~s^{-1})"),
         color = if_else(key == "NEP" & value > 0, 
                         "dark green", 
                         "black"))

df_met_l$type_plot <- factor(df_met_l$type_plot,
                             levels = c("GPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "ER~(g~O[2]~m^{-2}~d^{-1})",
                                        "NEP~(g~O[2]~m^{-2}~d^{-1})",
                                        "k[600]~(d^{-1})",
                                        "Discharge~(m^3~s^{-1})"))

# Load macrophyte data and simplify
df_mac <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>% #outlier species
  add_tally(surface_area) %>%
  mutate(per = surface_area / n) %>% #only want to name dominant species in legend
  mutate(species2 = ifelse(per < 0.11 | species %in% c("Ludwigia grandiflora",
                                                       "Najas marina",
                                                       "Elodea canadensis",
                                                       "Potamogeton nodosus",
                                                       "Potamogeton pectinatus",
                                                       "Spirodela polyrhiza",
                                                       "Lemna minor",
                                                       "Potamogeton perfoliatus"),
                           "other", species),
         type2 = ifelse(type == "floating_submerged", "submerged", type))
df_mac$species2 <- factor(df_mac$species2,
                       levels = c("Ranunculus fluitans", "Myriophyllum spicatum",
                                       "Vallisneria spiralis", "other"))

# Data analysis and seasonality -----------------------------------------------------------
# Some data summaries. Change all negatives to NA
# Change of labs in 2009–2011 where MDL increased to 0.1, so taking half of that
# Also need to change PO4 to PO4-P and NO3 to NO3-N
df_mid_wide <- df_mid_wq %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  mutate(PO4 = if_else(between(year, 2009, 2011) & PO4 == 0.1, 0.05, PO4),
         PO4 = PO4 * 30.97 / 94.97,
         NO3 = NO3 * 14.01/ 62)

# Estimate PO4 when it's below the MDL for the most recent era with linear mod
# Quick plot
ggplot(filter(df_mid_wide, between(TP, 0, 0.1),
              PO4 != 0.05* 30.97 / 94.97,
              year > 2008,
              site_no == "04048000", between(month, 4, 10)),
       aes(x = TP,
           y = PO4,
           color = year,
           shape = site_no)) + geom_point() +
  stat_smooth(method = "lm")
# Data for linear model
lm_dat <- filter(df_mid_wide, between(TP, 0, 0.1), 
                 PO4 != 0.05* 30.97 / 94.97,
                 year > 2008, 
                 site_no == "04048000", between(month, 4, 10))
# PO4 TP linear model for interpolation
p_lm <- lm(lm_dat$PO4~lm_dat$TP)

# Same thing for BOD5 and CHLA
ggplot(filter(df_mid_wide,
              !between(year, 2007, 2012),
              site_no == "04048000", between(month, 4, 10)),
       aes(x = CHLA,
           y = BOD5,
           color = year,
           shape = site_no)) + geom_point() +
  stat_smooth(method = "lm")
# Linear model
lm_dat2 <- filter(df_mid_wide, 
                  !between(year, 2007, 2012),
                  site_no == "04048000", between(month, 4, 10))
# BOD5 CHLA linear model for interpolation
p_lm2 <- lm(lm_dat2$BOD5~lm_dat2$CHLA)

# Estimate BOD5, PO4 when below MDL for years 2008-2011
df_mid_long <- df_mid_wide %>%
  mutate(PO4 = if_else((PO4 == (0.1* 30.97 / 94.97) | 
                          PO4 == (0.05* 30.97 / 94.97)) & 
                         !is.na(TP),
                       p_lm$coefficients[1] + p_lm$coefficients[2] * TP,
                       PO4),
         BOD5 = if_else(BOD5 == 2 &
                          between(year, 2008, 2011) &
                          !is.na(CHLA),
                        p_lm2$coefficients[1] + p_lm2$coefficients[2] * CHLA,
                        BOD5),
         Ppar = TP - PO4,
         NP = NO3 / PO4 * (31/14)) %>%
  pivot_longer(names_to = 'solute', values_to = "value", 
               cols = c(temp:ChOD, NP:Ppar)) %>%
  filter(!(solute == "TP" & value > 0.7))

# Save this data
# saveRDS(df_mid_long, "Data/Loire_DO/middle_loire_wq")

# Clean the data by removing NAs
df_mid_clean <- df_mid_long %>%
  filter_at(vars(value), all_vars(!is.na(.)))

# Data for plots with monthly averages (get rid of one weird data point for spm)
df_solutes <- df_mid_clean %>%
  dplyr::filter(solute %in% c("BOD5", "CHLA", 
                              "PO4", "NO3", "SPM"),
                !(solute == "SPM" & year == 1982)
                ) %>%
  group_by(solute, year, month) %>%
  summarize(date = mean(date),
            month_mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(summer = if_else(between(month, 4, 10), "summer", "winter"),
         month_mean = if_else(solute == "CHLA", month_mean / 1000, month_mean)) #for mg/L

# Calculate monthly running 90-day means of DO per. sat. min and max
df_do_rm <- df_do %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max = max(DO_per, na.rm = TRUE),
            min = min(DO_per, na.rm = TRUE)) %>%
  ungroup() %>%
  bind_rows(read_xlsx("Data/do_min_max_1990_1992.xlsx") %>%
              select(min = do_min,
                     max = do_max,
                     do_sat = do_sat,
                     date) %>%
              mutate(date = date(date),
                     # DO_sat = ifelse(temp <= 0,
                     #                 0,
                     #                 14.652 - 0.41022 * temp + 0.007991 * 
                     #                   temp^2 - 0.000077774 * temp^3),
                     max = max / do_sat,
                     min = min / do_sat)
            ) %>%
  arrange(date) %>%
  mutate(max = ifelse(is.infinite(max), NA, max),
         min = ifelse(is.infinite(min), NA, min),
         rm_max_wk = rollapply(max, width=90, FUN=function(x)
           mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE,
           fill=NA, align="right"),
         rm_min_wk = rollapply(min, width=90, FUN=function(x)
           mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE,
           fill=NA, align="right")) %>%
  pivot_longer(cols = c(rm_max_wk, rm_min_wk))
  
# Calculate annual values of corbicula with standard error
df_cor_sum <- df_cor %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(density = mean(dens, na.rm = TRUE),
            se = sd(dens, na.rm = TRUE) / sqrt(n())) %>%
  mutate(date = ymd(paste0(year, "-07-01")))

# Breakpoint analysis -----------------------------------------------------
# Estimate breakpoints
# Get nested solute data first
solute_bp_data_n <- df_mid_long %>%
  filter(year > 1979,
         solute %in% c("BOD5", "CHLA", "NO3", "PO4", "SPM")) %>%
  arrange(date) %>%
  group_by(year, month, solute) %>%
  distinct(date, .keep_all = TRUE) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(solute) %>%
  transmute(date = ymd(paste(year, month, "01", sep = "-")),
            value = mean) %>%
  nest()

# Same for metabolism data
met_bp_data_n <- df_met_l %>%
  arrange(date) %>%
  group_by(key) %>%
  nest()

# Short function for converting to time series (weekly mean for metabolism)
ts_conv <- function(data, log = TRUE, type = "solute"){
  data = if(log){
    data %>%
      na.trim() %>%
      mutate(value = log(value)) %>%
      mutate(value = ifelse(is.nan(value), NA, value)) %>%
      as.data.frame(.)
  } else {
    data %>%
      na.trim() %>%
      mutate(value = ifelse(is.nan(value), NA, value)) %>%
      as.data.frame(.)
  }
  if(type == "solute"){
    dat_ts = xts(x = data[, -1],
                 order.by = data[, 1])
  } else{ 
    dat_ts = data#data %>%
      # filter(between(month(date), 4, 10))
      # mutate(year = year(date), week = week(date)) %>%
      # group_by(year, week) %>%
      # summarize(median = median(value, na.rm = TRUE))
  }
  dat_ts = as_tibble(na_kalman(dat_ts)) %>% #interpolate with kalman for NAs
    mutate(ind = row_number())
  # dat_ts = as_tibble(na_interpolation(dat_ts, option = "stine")) %>%
  #   mutate(ind = row_number())
  # dat_ts = as.ts(dat_ts)
  # tibble(V1 = na_interpolation(dat_ts$median, option = "stine")) %>%
  # mutate(ind = row_number())
}

# Turn into time series and analyze for breakpoints
# First set the model structure for breakpoints, which includes
# a break in autocorrelation, intercept, variability, and slope
model <- list(
  V1 ~ 1 + sigma(1),  
  ~ 1 + sigma(1)
)
# Do the breakpoint analysis
# Quick prior of breakpoints based on previous analyses
bp_priors <- tibble(solute = c("BOD5", "CHLA", "NO3", "PO4", "SPM"),
                    bp_summer = c("dunif(130,210)",
                                  "dunif(130,210)",
                                  "dunif(130,210)",
                                  "dunif(130,210)",
                                  "dunif(130,210)"),
                    bp_all = c("dunif(200,320)",
                               "dunif(200,320)",
                               "dunif(200,320)",
                               "dunif(200,320)",
                               "dunif(200,320)"))

# Do bayesian breakpoint analysis
solute_bps <- solute_bp_data_n %>%
  left_join(bp_priors) %>%
  mutate(data_summer = future_map(data, ~filter(., between(month(date), 4, 10))),
         ts = future_map(data_summer, ts_conv),
         fits = map2(ts, bp_summer, ~mcp(model = model, data = .x, 
                              prior = list(cp_1 = .y), par_x = "ind")),
         mcp_res = map(fits, summary))

# Get the output
solute_bp_out <- solute_bps %>%
  select(solute, data_summer, mcp_res) %>%
  unnest(mcp_res) %>%
  select(solute, data_summer, name, mean, upper, lower) %>%
  filter(name == "cp_1") %>%
  pivot_longer(cols = c(-solute, -name, -data_summer),
               names_to = "type", values_to = "value") %>%
  mutate(dates = future_map2(data_summer, value, slice)) %>%
  hoist(dates,
        brkdate = "date") %>%
  ungroup() %>%
  select(-data_summer, -dates, -value, -name) %>%
  pivot_wider(names_from = type, values_from = brkdate)

# Some more details
solute_bp_out2 <- solute_bps %>%
  select(solute, data, mcp_res) %>%
  unnest(mcp_res) %>%
  select(solute, name, mean, upper, lower) %>%
  pivot_wider(names_from = c(name), values_from = c(mean, upper, lower))

# Estimate summertime values, means, sd, se, need to convert from log, also scale
solute_bp_values <- solute_bp_out %>%
  mutate(brkyr = year(round_date(mean, "year"))) %>%
  left_join(df_solutes %>%
              filter(between(month, 4, 10))) %>%
  mutate(period = if_else(date < mean, 1, 2)) %>%
  group_by(solute, period) %>%
  mutate(mlog = log(month_mean)) %>%
         # mlog_scale = rescale(mlog)) %>%
  summarize(mean = mean(mlog, na.rm = TRUE),
            sd = sd(mlog, na.rm = TRUE),
            ml = exp(mean + 0.5*sd^2),
            sdl = ml*sqrt(exp(sd^2)-1),
            sel = sdl / sqrt(n()))
            # mean_s = mean(mlog_scale, na.rm = TRUE),
            # sd_s = sd(mlog_scale, na.rm = TRUE),
            # ml_s = exp(mean_s + 0.5*sd^2),
            # sdl_s = ml_s*sqrt(exp(sd_s^2)-1),
            # sel_s = sdl_s / sqrt(n()))
#%>%
  # pivot_wider(names_from = period,
  #             values_from = c(mean, sd, ml, sdl)) #%>%
  # ungroup() %>%
  # mutate(yvals = c(14, 185, 4.2, 0.23, 135))

# Changepoints for DO max and min
# first get priors
bp_priors_do <- tibble(name = c("rm_max_wk", "rm_min_wk"),
                    bp_summer = c("dunif(4500,5500)",
                                  "dunif(4500,5500)"),
                    bp_all = c("dunif(7000,9000)",
                               "dunif(7000,9000)"))
# Calculate DO changepoints
do_bps <- df_do_rm %>%
  select(name, date, value) %>%
  group_by(name) %>%
  nest() %>%
  left_join(bp_priors_do) %>%
  mutate(data_summer = future_map(data, ~filter(., between(month(date), 4, 10))),
         ts = future_map(data, ts_conv, log = FALSE),
         fits = map2(ts, bp_all, ~mcp(model = model, data = .x, 
                                         prior = list(cp_1 = .y), par_x = "ind")),
         mcp_res = map(fits, summary))

# Get output in tidier format
do_bp_out <- do_bps %>%
  select(do_name = name, data, mcp_res) %>%
  unnest(mcp_res) %>%
  select(do_name, data, name, mean, upper, lower) %>%
  filter(name == "cp_1") %>%
  pivot_longer(cols = c(-do_name, -name, -data),
               names_to = "type", values_to = "value") %>%
  mutate(dates = future_map2(data, value, slice)) %>%
  hoist(dates,
        brkdate = "date") %>%
  ungroup() %>%
  select(-data, -dates, -value, -name) %>%
  pivot_wider(names_from = type, values_from = brkdate)

# Also want to rename somethign for later plotting
df_do_rm <- rename(df_do_rm, do_name = name) 

# DO changepoint with different method to double check
do_cpts <- df_do_rm %>%
  select(date, min, max) %>%
  pivot_longer(-date) %>%
  group_by(name) %>%
  nest() %>%
  mutate(data = future_map(data, na_kalman),
         ts = future_map(data, ~as.vector(.$value)),
         cps = future_map(ts, cpt.meanvar),
         cpt = future_map(cps, pluck, cpts),
         ests = future_map(cps, pluck, param.est),
         dates = future_map2(data, cpt, slice)) %>%
  select(name, dates, ests) %>%
  hoist(dates,
        brkdate = "date") %>%
  ungroup() %>%
  unnest_wider(ests) %>%
  hoist(variance,
        variance1 = 1L,
        variance2 = 2L) %>%
  ungroup() %>%
  hoist(mean,
        mean1 = 1L,
        mean2 = 2L) #%>%
  # select(-dates) %>%
  # transmute(name = name,
  #           brkyr = year(round_date(brkdate, "year"))) %>%
  # left_join(df_do_rm) %>%
  # mutate(period = if_else(year(date) < brkyr, 1, 2)) %>%
  # filter(between(month(date), 4, 9)) %>%
  # group_by(name, period, brkyr) %>%
  # summarize(mean = mean(value, na.rm = TRUE),
  #           sd = sd(value, na.rm = TRUE)) %>%
  # pivot_wider(names_from = period,
  #             values_from = c(mean, sd)) %>%
  # ungroup() %>%
  # mutate(brkdate = ymd(paste0(brkyr, "-06-01")),
  #        yvals = 200) %>%
  # filter(name != "rm_min_wk")

# Changepoints for metabolism
# new model form
model_met <- list(
  value ~ 1 + sigma(1),  # int_1, ar_1
  ~ 1 + sigma(1) # time_2, ar1_2
)

# Quick prior of breakpoints based on previous analyses
bp_priors_met <- tibble(key = c("GPP", "ER", "K600.daily", "discharge.daily", "NEP"),
                    bp = c("dunif(6000,9000)",
                           "dunif(6000,9000)",
                           "dunif(6000,9000)",
                           "dunif(6000,9000)",
                           "dunif(6000,9000)"))
# Do the bayesian changepoint analysis on weekly data (faster than daily, same result)
met_bps <- met_bp_data_n %>%
  left_join(bp_priors_met) %>%
  filter(key %in% c("GPP", "ER")) %>%
  mutate(ts = future_map(data, ts_conv, log = FALSE, type = "met"),
         fits = map2(ts, bp, ~mcp(model = model_met, prior = list(cp_1 = .y),
                             data = .x, par_x = "ind")),
         mcp_res = map(fits, summary))

# function to get date from week and year
# weekfun <- function(data){
#   data %>%
#     # filter(!(year(date) %in% c(1993, 2007))) %>%
#     mutate(year = year(date), week = week(date)) %>%
#     distinct(year, week) %>%
#     mutate(date = ymd(paste0(year,"-01-01")) + weeks(week - 1))
# }

# Get the output
met_bp_out <- met_bps %>%
  select(key, data, mcp_res) %>%
  unnest(mcp_res) %>%
  select(key, data, name, mean, upper, lower) %>%
  filter(name == "cp_1") %>%
  pivot_longer(cols = c(-key, -name, -data),
               names_to = "type", values_to = "value") %>%
  mutate(value = round(value)) %>%
  mutate(dates = future_map2(data, value, slice)) %>%
  hoist(dates,
        brkdate = "date") %>%
  ungroup() %>%
  select(-data, -dates, -value, -name) %>%
  pivot_wider(names_from = type, values_from = brkdate)

# Estimate summer values for metabolism for different periods
met_bp_values <- met_bp_out %>%
  select(key, mean) %>%
  # mutate(brkyr = year(round_date(mean, "year"))) %>%
  left_join(df_met_l %>%
              filter(between(month(date), 4, 10))) %>%
  mutate(period = if_else(date < mean, 1, 2)) %>%
  group_by(key, type_plot, mean, period) %>%
  summarize(mean_flux = mean(value, na.rm = TRUE),
            sd_flux = sd(value, na.rm = TRUE),
            se_flux = sd_flux / sqrt(n()))

# Double check with different method
met_cpts <- df_met %>%
  select(date, GPP, ER) %>%
  filter(between(month(date), 4, 10)) %>% # just look at summer, best estimates, only time when differences exist, anyway
  pivot_longer(-date) %>%
  group_by(name) %>%
  nest() %>%
  mutate(data = future_map(data, na_kalman),
         ts = future_map(data, ~as.vector(.$value)),
         cps = future_map(ts, cpt.meanvar),
         cpt = future_map(cps, pluck, cpts),
         ests = future_map(cps, pluck, param.est),
         dates = future_map2(data, cpt, slice)) %>%
  select(name, dates, ests) %>%
  hoist(dates,
        brkdate = "date") %>%
  ungroup() %>%
  unnest_wider(ests) %>%
  hoist(variance,
        variance1 = 1L,
        variance2 = 2L) %>%
  ungroup() %>%
  hoist(mean,
        mean1 = 1L,
        mean2 = 2L)

# Changepoints for corbicula and macrophtyes
mac_bp_data <- as.data.frame(df_mac %>%
                     group_by(year) %>%
                     summarize(area = sum(surface_area)) %>%
                   ungroup() %>%
                   select(area))
summary(cpt.mean(mac_bp_data$area)) #breakpoint is 2008/2009
mac_bp_values <- mac_bp_data %>%
  mutate(period = if_else(year < 2008, 1, 2),
         area = area / 7200) %>%
  group_by(period) %>%
  summarize(mean = mean(area),
            sd = sd(area))

cor_bp_data <- mutate(df_cor_sum,
                       d = if_else(is.infinite(log(density)), 0, log(density)))
summary(cpt.mean(cor_bp_data$d)) #breakpoint is 2001
cor_bp_values <- cor_bp_data %>%
  mutate(period = if_else(year < 2001, 1, 2)) %>%
  group_by(period) %>%
  summarize(mean = mean(density),
            sd = sd(density),
            se = sd / sqrt(n()))
# Figure 1 main plot -----------------------------------------------------------
# Chlorophyll a plot
# Reorder solutes and name for plotting
df_solutes$solute <- factor(df_solutes$solute,
                            levels = c("BOD5",
                                       "SPM",
                                       "PO4",
                                       "NO3",
                                       "CHLA"
                                       ))
levels(df_solutes$solute) <- c("BOD[5]",
                               "TSS",
                               "PO[4]^{`3-`}-P",
                               "NO[3]^{`-`}-N",
                               "Chlorophyll~a")

# Labels for solute plot
solute_labels <- df_solutes %>%
  filter(!is.na(solute)) %>%
  group_by(solute) %>%
  mutate(lab_date = ymd("1995-01-01"), lab_value = max(month_mean, na.rm = TRUE)) %>%
  distinct(solute, lab_date, lab_value)

# Rename changepoints for plotting
solute_bp_out$solute <- factor(solute_bp_out$solute,
                               levels = c("BOD5",
                                          "SPM",
                                          "PO4",
                                          "NO3",
                                          "CHLA"))
levels(solute_bp_out$solute) <- c("BOD[5]",
                                  "TSS",
                                  "PO[4]^{`3-`}-P",
                                  "NO[3]^{`-`}-N",
                                  "Chlorophyll~a")
solute_bp_out <- left_join(solute_bp_out, solute_labels, by = "solute")
# Same for this
solute_bp_values$solute <- factor(solute_bp_values$solute,
                                  levels = c("BOD5",
                                             "SPM",
                                             "PO4",
                                             "NO3",
                                             "CHLA"
                                  ))
levels(solute_bp_values$solute) <- c("BOD[5]",
                                     "TSS",
                                     "PO[4]^{`3-`}-P",
                                     "NO[3]^{`-`}-N",
                                     "Chlorophyll~a")

# Get x scale for all plots
brks <- seq(ymd("1990-01-01"), ymd("2020-01-01"), "1 year")
lbls <- format(seq(ymd("1990-01-01"), ymd("2020-01-01"), "5 years"), "%Y")
labs_yrs <- c(sapply(lbls, function(x) {
  c(x, rep("", 4))
  }))
labs_yrs <- labs_yrs[-(32:35)]
# Macrophyte plot
p_mac <- ggplot(data = filter(df_mac, species != "Elodea nuttallii"),
                aes(x = year,
                    y = surface_area / 7200)) + #per meter square over 7200 m2
  geom_bar(aes(fill = species2), stat = "identity") +
  stat_summary(aes(linetype = type2), fun.y = "sum", geom = "line") + 
  scale_x_continuous(limits = c(1993, 2018),
                     breaks = seq(1993, 2018, 1)) +
  geom_vline(aes(xintercept = 2008),
             linetype = "longdash",
             color = "dark green",
             size = 0.5,
             alpha = 0.8) +
  annotate(geom = "text", label = "2008", x = 2007, y = 0.12, 
           color = "dark green", size = 2.2) +
  # annotate(geom = "text", label = "e", x = 1994, y = 0.12, 
  #          color = "black", size = 2.5) +
  scale_fill_brewer(name = "species", type = "qual", palette = "Accent") +
  scale_linetype_discrete(name = "macrophyte type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.22, 0.65),
        legend.key.height = unit(0.2, "cm"),
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = -0.1, unit='cm'),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  xlab("") +
  ylab(expression("macrophyte cover ("*m^{2}~m^{-2}*")"))
p_mac

# Plot corbicula data
p_c <- ggplot(data = df_cor_sum,
              aes(x = date,
                  y = density)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = density - se,
                    ymax = density + se)) +
  geom_vline(aes(xintercept = ymd("2001-06-01")),
             linetype = "longdash",
             color = "black",
             size = 0.5,
             alpha = 0.8) +
  annotate(geom = "rect",
           xmin = ymd("2001-01-01"),
                xmax = ymd("2002-01-01"),
                ymin = 10^-2,
                ymax = 10^4,
            fill = "black",
            alpha = 0.4) +
  annotate(geom = "segment",
           x = ymd("1993-01-01"),
           xend = ymd("2018-12-31"),
           y = 1/(250*7500*1.4e-7/120),
           yend = 1/(250*7500*1.4e-7/120),
           color = "black",
           linetype = "dotted") +
  annotate(geom = "text",
           x = ymd("2008-01-01"),
           y = 920,
           color = "black",
           label = "approximate 100% turnover",
           size = 2.5) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  scale_y_log10(sec.axis = sec_axis(~ . *250*7500*1.4e-7/120, #m3/s filtration in 7.5 km reach divided by median summer discharge 
                                         name = "turnover ratio (-)",
                                    labels = trans_format("log10", math_format(10^.x)),
                                    breaks = c(10^-4, 10^-3, 10^-2, 10^-1,
                                               10^0, 10^1)),
                labels = trans_format("log10", math_format(10^.x)),
                breaks = c(10^-1, 10^0, 10^1, 10^2,
                           10^3),
                limits = c(10^-2, 10^4),
                expand = c(0, 0)) +
  annotation_logticks(sides="l") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        legend.position = c(0.2, 0.6),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  # labs(subtitle = "C") +
  xlab("") +
  ylab(expression(italic(Corbicula)~"(ind "*m^{-2}*")"))
p_c

# Plot DO data
p_do <- ggplot() + 
  geom_line(data = df_do_rm,
            aes(x = date,
                y = value * 100, #for % saturation
                color = do_name)) +
  geom_vline(data = do_bp_out,
             aes(xintercept = mean,
                 color = do_name),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8) +
  geom_rect(data = do_bp_out,
            aes(xmin = lower,
                xmax = upper,
                ymin = -Inf,
                ymax = Inf,
                fill = do_name),
            alpha = 0.4,
            show.legend = FALSE) +
  annotate(geom = "text",
           x = ymd("2010-07-01"),
           y = 200,
           color = "dark blue",
           label = "2011",
           size = 2.5) +
  # geom_vline(data = chapts_do, 
  #            aes(xintercept = brkdate),
  #            linetype = "longdash",
  #            color = "dark blue",
  #            size = 0.5,
  #            alpha = 0.8) +
  # geom_text(data = chapts_do, 
  #           aes(x = brkdate,
  #               y = yvals,
  #               label = brkyr),
  #           size = 2.2,
  #           show.legend = FALSE) +
  # geom_text(data = chapts_do, 
  #           aes(x = ymd("1987-01-01"),
  #               y = yvals,
  #               label = paste0("list(","`[`*",
  #                              "bar(summer[1])*",
  #                              "`]`==",
  #                              # "*",
  #                              round(mean_1*100, 0), 
  #                              "%+-%",
  #                              round(sd_1*100, 0),
  #                              ")")),
  #           parse = TRUE,
  #           size = 2.2,
  #           show.legend = FALSE) +
  # geom_text(data = chapts_do,  
  #           aes(x = ymd("2014-01-01"),
  #               y = yvals,
  #               label = paste0("list(","`[`*",
  #                              "bar(summer[2])*",
  #                              "`]`==",
  #                              # "*",
  #                              round(mean_2*100, 0), 
  #                              "%+-%",
  #                              round(sd_2*100, 0),
  #                              ")")),
  #           parse = TRUE,
  #           size = 2.2,
  #           show.legend = FALSE) +
  scale_color_manual(name = "90-d running mean",
                        breaks = c("rm_max_wk", "rm_min_wk"),
                        labels = c("daily max", "daily min"),
                        values = c("dark blue", "light blue")) +
  scale_fill_manual(name = "90-d running mean",
                     breaks = c("rm_max_wk", "rm_min_wk"),
                     labels = c("daily max", "daily min"),
                     values = c("dark blue", "light blue")) +
  scale_x_date(limits = c(ymd("1990-01-01"), ymd("2020-01-01")),
                   breaks = brks,
                   labels = labs_yrs) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.12, 0.85),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.1))) +
  xlab("") +
  ylab(expression("DO saturation (%)"))
p_do

# Add breakpoints to solutes
# Plot of monthly solute time series
p_solutes <- ggplot(data = df_solutes,
                     aes(color = solute,
                         fill = solute)) + 
  geom_line(aes(x = date,
                y = month_mean,
                group = 1,
                alpha = summer)) +
  geom_vline(data = solute_bp_out %>%
               dplyr::select(solute, mean), 
             aes(xintercept = mean),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8) +
  geom_rect(data = solute_bp_out,
            aes(xmin = lower, 
                xmax = upper, 
                ymin = -Inf, 
                ymax = Inf),
            alpha = 0.4) +
  geom_text(data = solute_bp_out,
            aes(x = mean + years(1),
                y = lab_value * 0.95,
                label = year(mean),
                color = solute),
            size = 2.5) +
  geom_text(data = solute_labels,
            aes(label = solute,
                x = lab_date,
                y = lab_value), 
            vjust = 1, 
            size = 2.5,
            parse = TRUE) +
  facet_wrap(~solute, scales = "free_y",
             labeller = label_parsed,
             ncol = 1) +
  scale_alpha_manual(name = "",
                     breaks = c("summer", "winter"),
                     values = c(1, 0.2)) +
  scale_color_manual(values = c("#EE0000FF", "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  scale_fill_manual(values = c("#EE0000FF",  "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  # labs(subtitle = "A") +
  xlab("") +
  ylab(expression("concentration (mg "~L^{-1}*")"))
p_solutes

# Plot of metabolism data
p_met <- ggplot() + 
  geom_line(data = filter(df_met_l, key %in% c("GPP", "ER")),
             aes(x = date,
                 y = value,
                 color = key,
                 group = key), alpha = 0.8, size = 0.25,
            show.legend = FALSE) +
  scale_color_manual(name = "",
                     # labels = parse_format(),
                     values = c("blue", "dark blue")) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  scale_y_continuous(limits = c(-26, 26)) +
  geom_hline(yintercept = 0) +
  geom_vline(data = filter(met_bp_out, key == "GPP") %>%
               dplyr::select(mean), 
             aes(xintercept = mean),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8,
             color = "dark blue") +
  geom_rect(data = filter(met_bp_out, key == "GPP"),
            aes(xmin = lower,
                xmax = upper,
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.4,
            fill = "dark blue",
            color = "dark blue") +
  geom_text(data = filter(met_bp_values, period == 2),
            aes(x = ymd("2015-01-01"),
                y = mean_flux * 3.4,
                label = "2014",
                color = key),
            size = 2.5,
            show.legend = FALSE) +
  annotate(geom = "text",
           x = ymd("1999-01-01"),
           y = 24,
           label = "GPP",
           color = "dark blue",
           size = 2.5) +
  annotate(geom = "text",
           x = ymd("1999-01-01"),
           y = -24,
           label = "ER",
           color = "blue",
           size = 2.5) +
  theme(axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # legend.position = c(0.15, 0.15),
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))) +
  # labs(subtitle = "D") +
  xlab("") +
  ylab(expression("metabolic flux"~(g~O[2]~m^{-2}~d^{-1})))
p_met

# Magnitude change plots --------------------------------------------------
# Solute magnitude change
p_mag_sol <- ggplot(data = solute_bp_values,
                    aes(x = as.factor(period),
                        y = ml,
                        color = solute)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = ml - sel, ymax = ml + sel),
                width = 0.1) +
  geom_blank(data = df_solutes %>% 
               select(solute, month_mean) %>%
               group_by(solute) %>%
               summarize(max = max(month_mean, na.rm = TRUE)) %>%
               right_join(solute_bp_values), 
             aes(y=max)) +
  geom_text(data = solute_bp_values %>%
              pivot_wider(names_from = period,
                          values_from = c(mean, sd, ml, sdl, sel)),
            aes(x = 1,
                y = ml_1 * 1.8,
                label = paste0("list(",#"`[`*",
                               #"bar(summer[1])*",
                               #"`]`==",
                               # "*",
                               round(ml_1, 2),
                               "%+-%",
                               round(sdl_1, 2),
                               ")")),
            parse = TRUE,
            size = 2.5) +
  geom_text(data = solute_bp_values %>%
              pivot_wider(names_from = period,
                          values_from = c(mean, sd, ml, sdl, sel)),
            aes(x = 2,
                y = ml_1 * 1.6,
                label = paste0("list(",#"`[`*",
                               #"bar(summer[2])*",
                               #"`]`==",
                               # "*",
                               round(ml_2, 2),
                               "%+-%",
                               round(sdl_2, 2),
                               ")")),
            parse = TRUE,
            size = 2.5) +
  facet_wrap(~solute, ncol = 1, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("#EE0000FF", "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))
p_mag_sol

# Metabolism change
# Quick refactor for plotting order
met_bp_values$key_f = factor(met_bp_values$key, 
                             levels=c('GPP','ER'))
p_mag_met <- ggplot(data = met_bp_values,
                    aes(x = as.factor(period),
                        y = mean_flux,
                        color = key_f)) +
  geom_blank(data = tibble(key_f = c("GPP", "ER"), fake_value = c(20, -20)) %>%
               mutate(key_f = factor(.$key_f, 
                      levels=c('GPP','ER'))) %>%
               right_join(met_bp_values), 
             aes(y=fake_value)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux),
                width = 0.1) +
  geom_text(data = met_bp_values %>%
              pivot_wider(names_from = period,
                          values_from = c(sd_flux, mean_flux, se_flux)),
            aes(x = 1,
                y = mean_flux_1 * 1.5,
                label = paste0("list(",#"`[`*",
                               #"bar(summer[1])*",
                               #"`]`==",
                               # "*",
                               round(mean_flux_1, 1),
                               "%+-%",
                               round(sd_flux_1, 1),
                               ")")),
            parse = TRUE,
            size = 2.5) +
  geom_text(data = met_bp_values %>%
              pivot_wider(names_from = period,
                          values_from = c(sd_flux, mean_flux, se_flux)),
            aes(x = 2,
                y = mean_flux_1 * 1.2,
                label = paste0("list(",#"`[`*",
                               #"bar(summer[2])*",
                               #"`]`==",
                               # "*",
                               round(mean_flux_2, 1),
                               "%+-%",
                               round(sd_flux_2, 1),
                               ")")),
            parse = TRUE,
            size = 2.5) +
  facet_wrap(~key_f, ncol = 1, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("dark blue", "blue")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))
p_mag_met

# Corbicula change
p_mag_cor <- ggplot(data = cor_bp_values,
                    aes(x = as.factor(period),
                        y = mean)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.1) +
  scale_y_log10(sec.axis = sec_axis(~ . *250*7500*1.4e-7/120, #m3/s filtration in 7.5 km reach divided by median summer discharge 
                                    name = "turnover ratio (-)",
                                    labels = trans_format("log10", math_format(10^.x)),
                                    breaks = c(10^-4, 10^-3, 10^-2, 10^-1,
                                               10^0, 10^1)),
                labels = trans_format("log10", math_format(10^.x)),
                breaks = c(10^-1, 10^0, 10^1, 10^2,
                           10^3),
                limits = c(10^-2, 10^4),
                expand = c(0, 0)) +
  # annotation_logticks(sides="rl") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        # axis.ticks.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"))
p_mag_cor

# Macrophyte change
p_mag_mac <- ggplot(data = mac_bp_values,
                    aes(x = as.factor(period),
                        y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.1) +
  # scale_y_continuous(limits = c(0,900)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"))
p_mag_mac
# Final plot and save -----------------------------------------------------
# Skip all analyses and load Rdata
load("C:/Users/jake.diamond/Desktop/Figure1_R_environment_data_new.RData")

(((p_solutes|p_mag_sol) +plot_layout(widths = c(6,1))) / 
   ((p_c|p_mag_cor) +plot_layout(widths = c(6,1))) /
   ((p_mac|p_mag_mac) +plot_layout(widths = c(6,1))) /
   ((p_met|p_mag_met) + plot_layout(widths = c(6,1))) +
   plot_layout(heights = c(4, 1, 1, 2)) +
    plot_annotation(tag_levels = 'a')) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure1_test_side_mags.png",
         device = "png",
         dpi = 300,
         height = 250,
         width = 183,
         units = "mm")

((p_met/ p_solutes1 /  p_c / p_mac / p_solutes2) + 
    plot_layout(heights = c(2, 2, 1, 1, 3))) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure1_met_first.png",
         device = "png",
         dpi = 300,
         height = 250,
         width = 183,
         units = "mm")



ggsave(plot = p_do,
       filename = "Figures/Middle_Loire/DO_breaks.png",
       device = "png",
       dpi = 300,
       height = 90,
       width = 183,
       units = "mm")
