# 
# Purpose: To summarize long-term data for middle Loire River
# Author: Jake Diamond
# Date: October 7, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(broom)
library(scales)
library(furrr)
library(hydrostats)
library(patchwork)
library(xts)
library(viridis)
library(zoo)
library(readxl)
library(changepoint)
library(imputeTS)
library(tidyverse)

load("Data/Loire_DO/Loire_data_all.RData")

# All loire discharge data
sites_q <- read_csv2("Data/Discharge/descriptifHydro2019.csv") %>%
  filter(str_detect(Nom, "La Loire")) %>%
  select(lat = Y,
         long = X,
         altitude = "Altitude (m)",
         site_code = Code,
         site_name = Nom)

# List all files
data_path <- "Data/Discharge/Export2019"
files <- dir(data_path, pattern = "*.txt")

# Read in all discharge data
df_q <- tibble(filename = files) %>%
  mutate(site = word(filename, 1, sep = "_")) %>%
  filter(site %in% sites_q$site_code) %>%
  mutate(file_contents = map(filename,
                             ~read_delim(file.path(data_path, .),
                                         delim = ";", skip = 5,
                                         col_names = FALSE))
  ) %>%
  unnest(cols = c(file_contents)) %>%
  select(-c(X, X1, X2, X5, X6, X7, filename), 
         date = X3,
         discharge = X4,
         site_code = site) %>%
  mutate(discharge = discharge / 1000,
         date = ymd(date)) %>%
  left_join(sites_q)

# get data for matt ross
df_mr <- tabPCAll %>%
  dplyr::select(site_no = cd_site,
                date = date_opecont,
                TSS = `MES`,
                PheoP = `PHEOPIG.`,
                CHLA = `CHL.A`) %>%
  mutate(date = as.Date(date)) %>%
  left_join(sLoire %>%
              select(site_no = cd_site,
                     site_name = site,
                     lat = y,
                     long = x)) %>%
  left_join()

saveRDS(df_q, file = "Data/Loire_DO/loire_discharge_matt_ross")
saveRDS(df_mr, file = "Data/Loire_DO/loire_wq_matt_ross")
  
# macrophyte dataset
df_mac <- left_join(cb1, lm1) %>%
  left_join(dplyr::select(stm, nom_taxon, cd_taxon)) %>%
  mutate(nom_taxon = str_trim(nom_taxon),
         genus = word(nom_taxon, 1)) %>%
  filter(support == "macrophyte") %>%
  select(site_no = cd_site,
         date = date_opecont,
         lotic_per_cover = reclotique,
         lentic_per_cover = reclentique,
         total_per_cover = recfu,
         taxon = nom_taxon,
         genus = genus
        ) %>%
  group_by(site_no, date) %>%
  summarize(lotic_per_cover = sum(lotic_per_cover),
            lentic_per_cover = sum(lentic_per_cover),
            total_per_cover = sum(total_per_cover))

# ggplot(data = df_mac,
#        aes(x = year(date_opecont),
#            y = recfu)) +
#   geom_bar(stat = "sum") +
#   facet_wrap(~cd_site)

# ggplot(data = filter(tabPC, cd_site %in% c("4048000", "4046800", "4045900", "4050500", "4050550"))) +
#          geom_line(aes(x = date_opecont,
#                    y = `Orthophosp`,
#                    color = as.factor(cd_site)))
  # geom_tile(aes(x = date_opecont,
  #               y = cd_site,
  #               fill = `NO3-`,
  #               color = `NO3-`),
  #             interpolate = TRUE)
  #             

# y <- filter(tabPC, cd_site %in% c("4048000", "4045900", "4050000")) %>%
#   select(val = `Orthophosp`, site = cd_site, date = date_opecont) %>%
#   pivot_wider(names_from = site, values_from = val) %>%
#   mutate(month = month(date),
#          year = year(date)) %>%
#   group_by(year, month) %>%
#   summarize_all(mean, na.rm = TRUE) %>%
#   mutate(del1 = (`4045900` - `4048000`) / 95,
#          del2 = (`4048000` - `4050000`) / 54) %>%
#   # filter(between(month, 4, 9)) %>%
#   ggplot() +
#   geom_point(aes(x = date,
#                 y = del1), color = "red") + 
#   geom_point(aes(x = date,
#                 y = del2)) + 
#   # geom_smooth(aes(x = date,
#   #                 y = diff)) +
#   scale_color_viridis_d() +
#   scale_y_continuous(limits = c(0, 0.4))
# # Set the plotting theme
# theme_set(theme_bw(base_size=7)+
#             theme(panel.grid.major = element_blank(), 
#                   panel.grid.minor = element_blank()))

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
  # drop_na() %>%
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

# Just middle Loire sites, join with discharge data
df_mid_wq <- filter(df, site_no %in% c("04048000",
                                    "04048100",
                                    "04049000",
                                    "04049850",
                                    "04050500",
                                    "04050000",
                                    "04050550",
                                    "04051000")) %>%
  left_join(readRDS("Data/dampierre_discharge_daily")) %>%
  distinct()

# Load long term DO and metabolism data
df_do <- readRDS("Data/all_DO_cleaned")
df_met <- readRDS("Data/Loire_DO/metabolism_results_all_years_constrainedK")
df_met_chin <- readRDS("Data/Loire_DO/metabolism_results_chinon_all_years_constrainedK_no_pool")
# df_met %>%
#   filter(between(date, ymd("2009-01-01"), ymd("2009-12-31"))) %>%
#   ggplot(aes(x = date, y = GPP)) + geom_point() + stat_smooth()+
#   scale_y_continuous(limits = c(0, 25))
# left_join(df_met, df_met_chin, by = "date") %>%
#   filter(between(date, ymd("2009-01-01"), ymd("2009-12-31"))) %>%
#   ggplot(aes(x = GPP.x, y = GPP.y)) + geom_point() + stat_smooth()+
#   scale_y_continuous(limits = c(0, 25)) + geom_abline(slope =1, intercept = 0)
# Load corbicula data
df_cor <- read_xlsx("Data/Loire_DO/corbicula.xlsx") %>%
  mutate(site = str_to_lower(site),
         er_est = 0.01 * density)

# Load discharge data
df_q <- readRDS("Data/dampierre_discharge_daily") %>%
  mutate(mon = month(date),
         mon2 = ifelse(mon > 9, mon%%10 + 1, mon + 3))

# Load macrophyte data
df_mac <- read_xlsx("Data/Loire_DO/macrophytes.xlsx") %>%
  pivot_longer(-year, names_to = "group", values_to = "surface_area")

# Data analysis and seasonality -----------------------------------------------------------
# Some data summaries. Change all negatives to NA
df_mid_ss <- df_mid_wq %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  mutate(PO4 = ifelse(between(year, 2009, 2011) & PO4 == 0.1, 0.05, PO4),
         NP = NO3 / PO4 * (31/14)) %>%
  pivot_longer(names_to = 'solute', values_to = "value", cols = c(temp:ChOD, NP))
df_mid_clean <- df_mid %>%
  filter_at(vars(discharge.daily, value), all_vars(!is.na(.)))

ggplot(df_mid_wq, aes(x = CHLA, y = BOD5)) + geom_point()

# (ggplot(data = df_mid) +
#     geom_point(aes(x = date,
#                    y = value,
#                    color = month),
#                alpha = 0.2) +
#       scale_y_log10() +
#     facet_wrap(~solute, scales = "free_y") +
#     scale_color_viridis_c(name = "Month") +
#     theme_bw() +
#     xlab("") +
#     ylab("Parameter value")) %>%
#   ggsave(.,
#          filename = "Figures/To share/Dampierre_solute_time_series_all_sites.tiff",
#          device = "tiff",
#          dpi = 300,
#          width = 30,
#          height = 19.4,
#          units = "cm")
#          
# Macrophyte plots

# Time series plots
# Data for plots with moving averages
df_solutes <- df_mid %>%
  dplyr::filter(solute %in% c("BOD5", "CHLA", 
                              "PO4", "NO3", "NP", "SPM",
                              "TP", "PheoP")) %>%
  group_by(solute, year, month) %>%
  summarize(month_mean = mean(value, na.rm = TRUE),
            month_max = max(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")),
         period = ifelse(year < 2004, 1, 2)) %>%
  group_by(solute
           # , period
           ) %>%
  mutate(rm12 = rollapply(month_mean, width=12, 
                          FUN=function(x)
                            mean(x, na.rm=TRUE), by=1, 
                          by.column=TRUE, partial=TRUE, 
                          fill=NA, align="center"),
         rmax12 = rollapply(month_max, width=12, 
                          FUN=function(x)
                            max(x, na.rm=TRUE), by=1, 
                          by.column=TRUE, partial=TRUE, 
                          fill=NA, align="center")
         # , month2 = ifelse(month > 9, month%%10 + 1, month + 3)
  ) %>%
  ungroup()

df_solutes$solute <- factor(df_solutes$solute,
                     levels = c("NO3",
                                "PO4",
                                "NP",
                                "CHLA",
                                "SPM",
                                "BOD5"))
levels(df_solutes$solute) <- c("NO[3]^{`-`}-N",
                               "PO[4]^{`3-`}-P",
                               "N:P",
                               "Chlorophyll~alpha",
                               "TSS",
                               "BOD[5]")


# Seasonality index
df_season <- df_solutes %>%
  group_by(year, solute) %>%
  mutate(ann_sum = sum(month_mean, na.rm = TRUE),
         mon_dif = abs(month_mean - ann_sum / 12)) %>%
  summarize(si = sum(mon_dif, na.rm = TRUE) / mean(ann_sum, na.rm = TRUE))

ggplot(data = df_season,
       aes(x = year,
           y = si)) +
  geom_point() + facet_wrap(~solute)

# winter over summer
df_ws <- df_solutes %>%
  mutate(
    season = case_when(
      month %in% 9:11 ~ "Fall",
      month %in%  6:8  ~ "Summer",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Winter")) %>%
  group_by(period, solute, season) %>%
  summarize(seas_mn = mean(month_mean, na.rm = TRUE),
            seas_var = var(month_mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = season, values_from = c(seas_mn, seas_var))

df_ws %>%
  mutate(ws = Winter / Summer,
         fs = Fall / Spring) %>%
  ggplot(aes(x = period, y = ws)) +
  geom_point() + facet_wrap(~solute, scales = 'free_y')

# Annual time cycle plots
(df_mid %>%
  filter(solute %in% c("PO4", "NO3", "NP", "CHLA", "SPM", "BOD5"),
         year > 1980) %>%
    mutate(date = ymd(paste(year, month, "01", sep = "-")),
           period = ifelse(year < 2004, 1, 2)) %>%
  ggplot(aes(x = month,
             y = value,
             color = period,
             group = interaction(period, month))) +
  stat_summary(fun.y = mean, geom = "line", aes(group= period)) +
  stat_summary(fun.y = median, geom = "point", aes(group= period)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2) +
  # stat_summary(fun.y = "mean", geom = "line", group = 1) +
  facet_wrap(~solute, scales = "free_y") +
  scale_color_viridis_c() +
  theme_bw(base_size=12)) %>%
  ggsave(filename = "Figures/Middle_Loire/seasonality_period.png",
         device = "png",
         width = 12,
         height = 6,
         units = "in",
         dpi = 300)

np <- df_mid %>%
  filter(solute %in% c("PO4", "NO3", "CHLA"),
         year > 1980) %>%
  group_by(solute, year, month) %>%
  summarize(month_mean = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = solute, values_from = month_mean) %>%
  mutate(NP = NO3 / PO4 * (31/14))
# Breakpoint analysis -----------------------------------------------------
# Estimate breakpoints
# Get nested data first
ts_dat <- df_mid %>%
  filter(year > 1978,
         solute %in% c("BOD5", "CHLA", "NO3", "PO4", "SPM")) %>%
  arrange(date) %>%
  group_by(year, month, solute) %>%
  distinct(date, .keep_all = TRUE) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  na.trim() %>%
  ungroup() %>%
  group_by(solute) %>%
  transmute(date = ymd(paste(year, month, "01", sep = "-")),
            value = mean) %>%
  nest()

# Short function for converting to time series
ts_conv <- function(data){
  data = as.data.frame(data)
  dat_ts = xts(x = data[, -1],
               order.by = data[, 1])
  dat_ts = na_interpolation(dat_ts, option = "stine")
  dat_ts = as.ts(dat_ts)
}

# Turn into time series and analyze
chapts <- ts_dat %>%
  mutate(ts = future_map(data, ts_conv),
         cps = future_map(ts, cpt.meanvar),
         cpt = future_map(cps, pluck, cpts),
         ests = future_map(cps, pluck, param.est),
         dates = future_map2(data, cpt, slice)) %>%
  select(solute, dates, ests) %>%
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
        mean2 = 2L) %>%
  select(-dates)

# Estimate summertime differences only
df_sums <- df_mid_clean

# Figure 2 plot -----------------------------------------------------------
# Macrophyte plot
library(scales)
p_mac <- ggplot(data = df_mac,
                aes(x = year,
                    y = surface_area)) +
  geom_bar(stat = "identity", na.rm=TRUE) +
  scale_x_continuous(limits = c(1980, 2018),
               breaks = seq(1980, 2018, 5)) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis_d() + 
  guides(stat = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(subtitle = "B") +
  xlab("") +
  ylab(expression("Macrophyte surface area ( "*m^{2}*")"))
p_mac

p_chla <- ggplot(data = dplyr::filter(df_solutes,
                                     between(month, 4, 10),
                                     solute %in% c("Chlorophyll~alpha"))) + 
  geom_line(aes(x = date,
                y = rm12)) +
  geom_line(aes(x = date,
                y = rmax12)) +
  # geom_vline(aes(xintercept = bp),
  #            linetype = "dotted") +
  # geom_text(aes(x = as.Date(bp),
  #               y = bpy,
  #               label = as.character(as.Date(bp)))) +
  # facet_wrap(~solute, scales = "free_y",
  #            labeller = label_parsed,
  #            ncol = 2) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  # scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
  #                      labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) + 
  guides(color = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(subtitle = "A") +
  xlab("") +
  ylab(expression("Chlorophyll"~alpha~"(mg "~L^{-1}*")"))
p_chla

# Add breakpoints to solutes
# Plot of monthly solute time series
p_solutes <- ggplot(data = dplyr::filter(df_solutes,
                                         between(month, 4, 10),
                                         solute %in% c("NO[3]^{`-`}-N",
                                                       "PO[4]^{`3-`}-P",
                                                       "TSS",
                                                       "BOD[5]"))) + 
  geom_line(aes(x = date,
                y = rm12)) +
  # geom_vline(aes(xintercept = bp),
  #            linetype = "dotted") +
  # geom_text(aes(x = as.Date(bp),
  #               y = bpy,
  #               label = as.character(as.Date(bp)))) +
  facet_wrap(~solute, scales = "free_y",
             labeller = label_parsed,
             ncol = 2) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  # scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
  #                      labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) + 
  guides(color = FALSE) +
  theme(axis.title.x = element_blank()) +
  labs(subtitle = "E") +
  xlab("") +
  ylab(expression("Concentration (mg "~L^{-1}*")"))
p_solutes

# Do plotting
# Only get dampierre data
df_do <- df_do %>%
  group_by(site) %>%
  distinct(datetime, .keep_all = TRUE) %>%
  arrange(datetime) %>%
  ungroup() %>%
  filter(site == "dampierre")

# Calculate weekly running means of min and max
df_do_rm <- df_do %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max = max(DO_use, na.rm = TRUE),
            min = min(DO_use, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(max = ifelse(is.infinite(max), NA, max),
         min = ifelse(is.infinite(min), NA, min),
         rm_max_wk = rollapply(max, width=24*7, FUN=function(x) 
           mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, 
           fill=NA, align="center"),
         rm_min_wk = rollapply(min, width=24*7, FUN=function(x) 
           mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, 
           fill=NA, align="center")) %>%
  pivot_longer(cols = c(rm_max_wk, rm_min_wk))

# Plot DO data
p_do <- ggplot() + 
  geom_point(data = df_do,
            aes(x = datetime,
                y = DO_use,
                color = month),
            alpha = 0.2) +
  # geom_line(data = df_do_rm,
  #           aes(x = ymd_h(paste0(date, "-00")),
  #               y = value,
  #               linetype = name),
  #           alpha = 0.8) +
  guides(color = FALSE) +
  # scale_linetype_manual(name = "Weekly mean",
  #                       breaks = c("rm_max_wk", "rm_min_wk"),
  #                       labels = c("max", "min"),
  #                       values = c("solid", "dashed")) +
  scale_x_datetime(limits = c(ymd_h("1993-01-01 00"), ymd_h("2019-01-01 00")),
                   breaks = seq(ymd_h("1990-01-01 00"), ymd_h("2020-01-01 00"), 
                                "5 years"),
                   labels = date_format("%Y")) +
  scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))) +
  labs(subtitle = "A") +
  xlab("") +
  ylab(expression("DO concentration (mg "*L^{-1}*")"))
p_do

# Plot discharge data
p_q <- ggplot(data = df_q,
              aes(x = date,
                  y = discharge.daily
                  # ,color = mon2
                  )) +
  geom_line() +
  scale_x_date(limits = c(ymd("1980-10-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  # scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
  #                      labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) +
  # guides(color = guide_colorbar(title = "Month",
  #                               title.position = "top",
  #                               direction = "horizontal",
  #                               frame.colour = "black",
  #                               barwidth = 6,
  #                               barheight = 0.5)) +
  theme(axis.title.x = element_blank(),
        # legend.position = c(0.8, 0.85),
        # legend.background = element_rect(fill=alpha("white", 0.4))
        ) +
  labs(subtitle = "D") +
  xlab("") +
  ylab(expression("Mean daily discharge ("*m^3~s^{-1}*")"))
p_q

# Plot corbicula data
p_c <- ggplot(data = filter(df_cor, site == "dampierre"),
              aes(x = ymd(paste0(date, "-01-01")),
                  y = `log(D+1)`)) + 
  geom_line() +
  geom_point() +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"),
                            "5 years"),
               labels = date_format("%Y")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(subtitle = "C") +
  xlab("") +
  ylab(expression("ln("*italic(Corbicula~sp.)~"density + 1)"))
p_c
# * plot_layout(heights = c(4, 1)
# Plot all plots
(((p_chla / p_mac / p_c / p_q) | (p_solutes)) + plot_layout(heights = c(4, 1),
                                                            widths = c(1, 1.5))) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure2_v2.png",
         device = "png",
         dpi = 300,
         height = 6,
         width = 7.25,
         units = "in")

# CQ analysis -------------------------------------------------------------
# Clean data for NAs to plot CQ
df_mid_clean <- df_mid %>%
  filter_at(vars(discharge.daily, value), all_vars(!is.na(.)))

(ggplot(data = df_mid_clean) + 
    geom_point(aes(x = discharge.daily,
                   y = value,
                   color = year),
               alpha = 0.2) +
    scale_y_log10() +
    scale_x_log10() +
    facet_wrap(~solute, scales = "free_y") + 
    scale_color_viridis_c() +
    theme_bw() + 
    xlab("Discharge (m3/s)") +
    ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_CQ_all_sites_v2.tiff",
         device = "tiff",
         dpi = 300,
         width = 30,
         height = 19.4,
         units = "cm")

df_mid %>%
  filter(solute == "PO4") %>%
  group_by(year, solute) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  tail()

(ggplot(data = df_mid, aes(x = year, y = value)) + 
    stat_summary(fun.y = mean, geom = "point") + 
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
    facet_wrap(~solute, scales = "free_y") +
    theme_bw() + 
    xlab("") +
    ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_time_series_annual_all_sites.tiff",
         device = "tiff",
         dpi = 300,
         width = 30,
         height = 19.4,
         units = "cm")

df_mid_pl <- df_mid_clean %>%
  group_by(site_no, solute) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(log(.$value)~log(.$discharge.daily)))) %>%
  mutate(glance_lm = map(mod, glance),
         rsq =map_dbl(glance_lm, "r.squared"),
         tidy_lm = map(mod, tidy)) %>%
  unnest(tidy_lm)


# Quick plot of BOD vs ChlA
df_mid_clean %>% 
  filter(solute %in% c("BOD5", "CHLA")) %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  ggplot(aes(x = CHLA, y = BOD5, 
             color = as.factor(year),
             group = year)) +
  geom_point() +
  # scale_color_viridis_c() +
  geom_smooth(method = "lm")


# Compare metabolism to WQ -----------------------------------------------

# Daily comparison
df_all <- left_join(df_met, df_mid_clean, by = "date") %>%
  left_join(df_do %>%
              filter(site == "dampierre") %>%
              mutate(date = date(datetime)) %>%
              group_by(date) %>%
              summarize(max = max(DO_use, na.rm = TRUE),
                        amp = diff(range(DO_use, na.rm = TRUE)))
  )


ggplot(filter(df_all, month == 5)) + geom_point(aes(x = value,
                                y = GPP)) +
  facet_wrap(~solute, scales = "free")

ggplot(left_join(df_met, df_q)) + geom_point(aes(x = discharge.daily,
                                                    y = GPP)) +
  facet_wrap(~year(date))
ggplot(filter(df_all, solute == "BOD5"), aes(x = value, y = ER)) + geom_point()
# Annual comparison
library(ggpmisc)
(df_all %>%
    # group_by(year) %>%
    # filter(solute == "CHLA",
    #        between(month,6,9)) %>%
    filter(solute == ("CHLA"),
           GPP > 0,
           year == 1999,
           site_no == "04050500",
           # site_no %in% c("04050500", "04050000", "04050550"),
           between(month, 5 ,10)) %>%
    mutate(rm3 = rollapply(GPP, width=3, 
                            FUN=function(x)
                              mean(x, na.rm=TRUE), by=1, 
                            by.column=TRUE, partial=TRUE, 
                            fill=NA, align="right"),
           rm3_amp = rollapply(amp, width=3, 
                           FUN=function(x)
                             mean(x, na.rm=TRUE), by=1, 
                           by.column=TRUE, partial=TRUE, 
                           fill=NA, align="right"),
           rm3_max = rollapply(max, width=3, 
                           FUN=function(x)
                             mean(x, na.rm=TRUE), by=1, 
                           by.column=TRUE, partial=TRUE, 
                           fill=NA, align="right")) %>%
    # summarize(gpp = median(GPP, na.rm = TRUE),
    #           val = median(value, na.rm = TRUE)) %>%
    ggplot(aes(x = value,
               y = GPP,
               color = year)) +
    geom_point() +
    geom_text(aes(label = year)) + 
    stat_smooth(method = "glm") +
    # stat_poly_eq(formula = y~x, 
    #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #              parse = TRUE,
    #              label.x = 0.8, label.y = 0.1) +
    theme_bw() +
    xlab("Median Chlorophyll a (mg/L)") +
    ylab("Summertime median GPP (g O2 m-2 d-1)")
) %>%
  ggsave(filename = "Figures/chla_vs_gpp_pool.tiff",
         device = 'tiff',
         dpi = 400)
plot(log(df$TP), log(df$CHLA))

# GPP vs ER
# with quant
library(quantreg)
?rq
(df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER)) %>%
  filter(between(month(date),4,10)) %>%
  ggplot(aes(x = GPP,
             y = ER)) +
  geom_point() +
  facet_wrap(~year(date)) +
  theme_bw() +
  stat_smooth(method = "lm") +
  geom_quantile(quantiles = 0.9) +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label..)),
               parse = TRUE,
               label.x = 0.8, label.y = 0.1)) %>%
  ggsave(filename = "Figures/Middle_Loire/quantile_regression.png",
         device = 'png',
         width = 19,
         height = 19,
         units = "cm",
         dpi = 300)
df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         year = year(date)) %>%
  filter(between(month(date),4,10)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(mod = future_map(data, ~rq(.$ER~.$GPP, tau = 0.9, data = .)),
         res = future_map(mod, tidy)) %>%
  unnest(res) %>%
  select(year, estimate, term) %>%
  dplyr::filter(term == ".$GPP") %>%
  left_join(filter(df_solutes, solute == "Chlorophyll~a",
                   between(month, 4, 10)) %>%
              group_by(year) %>%
              summarize(mean = mean(month_mean, na.rm = TRUE))) %>%
  ggplot(aes(x = mean,
             y = estimate)) + 
  geom_point()
  
# Recovery time -----------------------------------------------------------
# Hydrostatistics
df_q_stat <- df_q %>%
  group_by(year(Date)) %>%
  nest() %>%
  mutate(ls = future_map(data, high.spells, threshold = 150)) %>%
  unnest(ls)
df_q_stat <- high.spell.lengths(df_q, threshold = 150) %>%
  mutate(end_date = start.date + days(spell.length))

ggplot(filter(df_q, year(Date) == 2018),
       aes(x = Date,
           y = Q)) + geom_point()

# Function for run length encoding
myrleid <- function(x) {
  x <- rle(x)$lengths
  rep(seq_along(x), times=x)
}
# delta df_q
df_q_stat <- df_q %>%
  mutate(del = Q - lag(Q),
         date = as.Date(Date),
         sign = sign(del),
         run = myrleid(sign)) %>%
  group_by(run) %>%
  mutate(length = cumsum(run))

q <- left_join(df_met, df_q_stat) %>%
  mutate(delgpp = GPP - lag(GPP),
         year = year(Date)) %>%
  filter(month(Date) == 7)

ggplot(left_join(df_met, df_q_stat) %>%
         mutate(delgpp = GPP - lag(GPP),
                year = year(Date)) %>%
         filter(month(Date) == 7)) + geom_point(aes(x = sign,
                                                 y = GPP,
                                                 color = day(Date))) +
  facet_wrap(~year) +
  # scale_y_continuous(limits = c(0, 25)) +
  # scale_x_continuous(limits = c(-250, 250)) +
  scale_color_viridis_c() +
  geom_vline(xintercept = 0)

ggplot(left_join(df_met, df_q_stat) %>%
         filter(month(Date) == 7)) + geom_point(aes(x = del,
                                                    y = GPP)) +
  # facet_wrap(~year(Date)) +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_vline(xintercept = 0)
# Breakpoints -------------------------------------------------------------
library(strucchange)
library(xts)
df_do_ts <- filter(df_do, site == "dampierre") %>%
  dplyr::select(datetime, filtered) %>%
  distinct(datetime, .keep_all = TRUE) %>%
  arrange(datetime) %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max = max(filtered, na.rm = TRUE),
            min = min(filtered, na.rm = TRUE),
            amp = diff(range(filtered, na.rm = TRUE))) %>%
  na.trim() %>% 
  mutate(max_s = rollapply(max, width=28, 
                           FUN=function(x)
                             mean(x, na.rm=TRUE), by=1, 
                           by.column=TRUE, partial=TRUE, 
                           fill=NA, align="center"),
         min_s = rollapply(min, width=28, 
                           FUN=function(x)
                             mean(x, na.rm=TRUE), by=1, 
                           by.column=TRUE, partial=TRUE, 
                           fill=NA, align="center"),
         amp_s = rollapply(amp, width=28, 
                           FUN=function(x)
                             mean(x, na.rm=TRUE), by=1, 
                           by.column=TRUE, partial=TRUE, 
                           fill=NA, align="center")) %>%
  group_by(cut(date, breaks = "14 days")) %>%
  summarize(max_s = mean(max_s, na.rm = TRUE),
            min_s = mean(min_s, na.rm = TRUE),
            amp_s = mean(amp_s, na.rm = TRUE)) %>%
  as.data.frame()
tp_ts <- xts(x = df_do_ts[, "amp_s"],
             order.by = ymd(df_do_ts[, 1]))
plot(tp_ts)
tp_ts <- na_interpolation(tp_ts, option = "stine")
plot(tp_ts)
tp_ts <- as.ts(tp_ts)
fs_tp <- Fstats(tp_ts ~ 1)
fs_tp_stats <- fs_tp$Fstats
plot(fs_tp)
sctest(fs_tp)

bp_tp <- breakpoints(tp_ts ~ 1,
                     engine = "C")
bp_ci <- confint(bp_tp)
plot(tp_ts)
lines(bp_ci)
df_do_ts$`cut(date, breaks = "14 days")`[bp_tp$breakpoints]


pacf((window(tp_ts, end = 283)))
pacf((window(tp_ts, start = 284)))
hist(tp_ts)
d <- ts.intersect(y = log(tp_ts), y1 = stats::lag(log(tp_ts), -1))
fs <- Fstats(y ~ y1, data = d)
plot(fs)
lines(breakpoints(fs))
sc <- efp(y ~ y1, data = d, type = "Score-CUSUM")
plot(sc, functional = NULL)
bp <- breakpoints(y ~ y1, data = d)
coef(bp)
plot(tp_ts, col = "lightgray", lwd = 2)
lines(fitted(bp))
lines(confint(bp))

library(earlywarnings)
ew <- generic_ews(tp_ts,
                  winsize = 50, 
                  # detrending = "first-diff",
                  logtransform = FALSE)


chla_ts_dat <- df_mid %>%
  filter(solute == "SPM") %>%
  group_by(solute, year, month) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  as.data.frame()

chla_ts = xts(x = chla_ts_dat[, "mean"],
             order.by = chla_ts_dat[, "date"])
chla_ts = na_interpolation(chla_ts, option = "stine")
chla_ts = as.ts(chla_ts)
plot(chla_ts)
fs_chla <- Fstats(chla_ts ~ 1)
fs_chla_stats <- fs_chla$Fstats
plot(fs_chla)
sctest(fs_chla)
sc <- efp(chla_ts ~ 1, type = "Score-CUSUM")
plot(sc, functional = NULL)
bp_chla <- breakpoints(log(chla_ts) ~ 1)
bp_chla
bd_chla <- breakdates(bp_chla)
bp_ci <- confint(bp_chla)
bp_ci
plot(chla_ts)
lines(bp_ci)
coef(bp_chla)
chla_ts_dat$date[bp_chla$breakpoints]
chla_ts_dat$date[bp_ci$confint]

pacf((window(chla_ts, end = 368)))
pacf((window(chla_ts, start = 368)))

d <- ts.intersect(y = chla_ts, y1 = stats::lag(chla_ts, -1))
fs <- Fstats(y ~ y1, data = d)
plot(fs)
lines(breakpoints(fs))
sc <- efp(y ~ y1, data = d, type = "Score-CUSUM")
plot(sc, functional = NULL)
bp <- breakpoints(y ~ y1, data = d)
coef(bp)
plot(log(chla_ts), col = "lightgray", lwd = 2)
lines(fitted(bp))
lines(confint(bp))
chla_ts_dat$date[bp$breakpoints]
library(earlywarnings)
ew <- generic_ews(chla_ts[1:bp$breakpoints],
                  winsize = 10, 
                  # detrending = "first-diff",
                  logtransform = FALSE)

# Metabolism dygraphs -----------------------------------------------------
library(dygraphs)
dy_data <- zoo::zoo(x = df_met[-c(1:2, 6)], 
                    order.by = df_met$date)

dy_g <- dygraph(dy_data,
        main = "Dampierre Metabolism",
        width=800,height=400) %>%
  dyOptions(drawGrid = F,
            useDataTimezone = TRUE) %>%
  # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
  #        independentTicks = TRUE,
  #        valueRange = c(0, 14))  %>%
  dySeries("GPP", label = "GPP") %>%
  dySeries("ER", label = "ER") %>%
  dySeries("NPP", label = "NEP") %>%
  dyAxis("y", label = "g O2 m<sup>2</sup> d<sup>-1</sup>",
         independentTicks = TRUE)  %>%
  dyRangeSelector()

htmltools::browsable(htmltools::tagList(dy_g))
,
valueRange = c(0, 130)

# Metabolism graphs -------------------------------------------------------
df_met_l <- df_met %>%
  ungroup() %>%
  select(-time_frame) %>%
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
  ggsave(filename = "Figures/Middle_Loire/metabolism_pool_facets.png",
         device = "png",
         dpi = 300,
         width = 18.4,
         height = 9.2,
         units = "cm")
library(ggrepel)
# Summer mean ER vs GPP
(df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 4, 10)) %>%
  group_by(year) %>%
  summarize(GPP_mean = mean(GPP, na.rm = TRUE),
            GPP_sd = sd(GPP, na.rm = TRUE),
            GPP_n= n(),
            ER_mean = mean(ER, na.rm = TRUE),
            ER_sd = sd(ER, na.rm = TRUE),
            ER_n= n()) %>%
  mutate(GPP_se = GPP_sd / sqrt(GPP_n),
         lower.ci.GPP = GPP_mean - qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
         upper.ci.GPP = GPP_mean + qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
         ER_se = ER_sd / sqrt(ER_n),
         lower.ci.ER = ER_mean - qt(1 - (0.05 / 2), ER_n - 1) * ER_se,
         upper.ci.ER = ER_mean + qt(1 - (0.05 / 2), ER_n - 1) * ER_se) %>%
  ggplot(aes(x = GPP_mean,
             y = ER_mean,
             color = year)) +
  geom_point() +
  geom_text_repel(aes(label = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  theme() +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
  ggtitle(label = "Summer mean ER vs. GPP")) %>%
  ggsave(filename = "Figures/Middle_Loire/summer_ER_vs_GPP.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 9,
         units = "cm")

df_summary <- df_met %>%
  ungroup() %>%
  dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER,
         year = year(date),
         month = month(date),
         period = ifelse(year >2011, 2, 1)) %>%
  # filter(between(month, 4, 9)) %>%
  group_by(year) %>%
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE))


  
(df_met %>%
  ungroup() %>%
  dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER,
         year = year(date),
         month = month(date)) %>%
  # filter(between(month, 4, 9)) %>%
  ggplot() +
    theme_bw(base_size = 6) +
    theme(panel.grid.minor = element_blank()) +
  stat_summary(aes(x = year, y = GPP),
                   fun.y = mean, geom = "point") +
  stat_summary(aes(x = year, y = GPP),
               fun.y = median, geom = "point", color = "red") +
  stat_summary(aes(x = year, y = GPP),
               fun.data = mean_cl_boot, geom = "errorbar") + 
  geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
              aes(x = year, y = GPP), method = "lm",
              alpha = 0.5) +
  stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
                             formula = y~x, 
               aes(x = year, y = GPP, 
                   label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,
               eq.with.lhs = "italic(GPP[summer])~`=`~",
               label.x = 0.8, label.y = 0.55,
               size = 2) +
  stat_summary(aes(x = year, y = ER),
               fun.y = mean, geom = "point") +
  stat_summary(aes(x = year, y = ER),
               fun.y = median, geom = "point", color = "red") +
  stat_summary(aes(x = year, y = ER),
               fun.data = mean_cl_boot, geom = "errorbar") +
  geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
              aes(x = year, y = ER), method = "lm",
              alpha = 0.5) +
  stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
               formula = y~x, 
               aes(x = year, y = ER, 
                   label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,
               eq.with.lhs = "italic(ER[summer])~`=`~",
               label.x = 0.8, label.y = 0.02,
               size = 2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(1994, 2018, 2)) +
  xlab("") +
  ylab(expression(flux~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/annual_flux_change.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")
  
# Overall Plot ------------------------------------------------------------



# Overall plot
p_q <- ggplot(data = df_q %>%
                group_by(year(Date), month(Date)) %>%
                summarize(med_q = median(Q, na.rm = FALSE)) %>%
                mutate(date = ymd(paste(`year(Date)`,
                                        `month(Date)`,
                                        "01",
                                        sep = "-"))),
              aes(x = date,
                  y = med_q)) + 
  scale_x_date(limits = c(ymd("1994-10-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) +
  guides(color = guide_colorbar(title = "Month",
                                title.position = "top",
                                direction = "horizontal",
                                frame.colour = "black",
                                barwidth = 6,
                                barheight = 0.5)) +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        axis.ticks.x = element_line()) +
  xlab("") +
  ylab(expression("Mean daily discharge ("*m^3~s^{-1}*")"))
p_q


df_send <- left_join(df_met, df_q, by = "date") %>%
  left_join(df_mid_wq %>%
              mutate_all(. , list(~na_if(., -1))) %>%
              mutate(NP = NO3 / PO4 * (31/14))) %>%
  left_join(read_excel("Data/Meteo/radiation_dampierre.xlsx") %>%
              select(site = NOM, datetime = DATE, light = GLO) %>%
              mutate(light = ifelse(is.na(light), 0,
                                    light * 10000*2.1/3600),
                     datetime = ymd_h(datetime),
                     date = date(datetime)) %>%
              filter(!(site == "SANCERRE" & datetime > ymd_h("2010-08-25-00"))) %>%
              select(-site) %>%
              group_by(date) %>%
              filter(light > 0) %>%
              summarize(med_light = median(light, na.rm = TRUE),
                        max_light = max(light, na.rm = TRUE)))
saveRDS(df_send, file = "Data/Loire_DO/all_data")
plot(df_send$med_light, df_send$GPP)

write_excel_csv(df_send, "Data/Loire_DO/all_data.csv")


# Time series decomposition -----------------------------------------------
# Time series analysis
gpp_ts <- df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
gpp_ts <- ts(gpp_ts$GPP, frequency = 365)
stl_gpp = stl(gpp_ts, "periodic")
seasonal_stl_gpp   <- stl_gpp$time.series[,1]
trend_stl_gpp     <- stl_gpp$time.series[,2]
random_stl_gpp  <- stl_gpp$time.series[,3]

plot(gpp_ts)
plot(as.ts(seasonal_stl_gpp))
plot(trend_stl_gpp)
abline(a = 4.5, b = -0.04)
plot(random_stl_gpp)
plot(stl_gpp)

summary(lm(trend_stl_gpp ~ index(seasonal_stl_gpp)))

# Only look at summer?
gpp_sum_ts <- df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  dplyr::filter(between(month(date), 4, 10),
                year(date) < 2004) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
gpp_sum_ts <- ts(gpp_sum_ts$GPP, frequency = 214)
stl_gpp_sum = stl(gpp_sum_ts, "periodic")
seasonal_stl_gpp_sum <- stl_gpp_sum$time.series[,1]
trend_stl_gpp_sum <- stl_gpp_sum$time.series[,2]
random_stl_gpp_sum <- stl_gpp_sum$time.series[,3]

plot(gpp_sum_ts)
plot(as.ts(seasonal_stl_gpp_sum))
plot(trend_stl_gpp_sum)
abline(a = 4.5, b = -0.04)
plot(random_stl_gpp_sum)
plot(stl_gpp_sum)
mk.test(trend_stl_gpp_sum)
summary(lm(trend_stl_gpp_sum ~ index(seasonal_stl_gpp_sum)))


# Discharge
q_ts <- df_q %>%
  dplyr::filter(between(date, ymd("1993-01-01"),
                        ymd("2018-12-31"))) %>%
  right_join(ungroup(df_met) %>%
               dplyr::select(date)) %>%
  ungroup() %>%
  dplyr::select(discharge.daily) %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
q_ts <- ts(q_ts[,1], frequency = 365)
stl_q <- stl(q_ts, "periodic")
seasonal_stl_q   <- stl_q$time.series[,1]
trend_stl_q     <- stl_q$time.series[,2]
random_stl_q  <- stl_q$time.series[,3]

plot(q_ts)
plot(as.ts(seasonal_stl_q))
plot(trend_stl_q)
abline(a = 4.5, b = -0.04)
plot(random_stl_q)
plot(stl_q)

summary(lm(trend_stl_q ~ index(seasonal_stl_q)))

# Get GPP and discharge trends in one dataframe
df_ts_p <- as.tibble(pluck(stl_q, "time.series")) %>%
  mutate(type = "Discharge",
         time = dplyr::row_number(),
         seasonal_scale = scale(seasonal),
         remainder_scale = scale(remainder),
         trend_scale = scale(trend)) %>%
  bind_rows(as.tibble(pluck(stl_gpp, "time.series")) %>%
              mutate(type = "GPP",
                     time = dplyr::row_number(),
                     seasonal_scale = scale(seasonal),
                     remainder_scale = scale(remainder),
                     trend_scale = scale(trend))) %>%
  left_join(mutate(gpp_ts, time = dplyr::row_number()) %>%
              dplyr::select(time, date)) %>%
  pivot_longer(c(seasonal_scale, remainder_scale, trend_scale))

df_ts_p_scale <- df_ts_p %>%
  scale()

(ggplot(df_ts_p,
       aes(x = date,
           y = value,
           color = type)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = "free_y") + 
    scale_color_manual(name = "",
                       values = c("blue",
                                  "red")) +
  scale_x_date(date_breaks = "1 years",
               limits = c(ymd("1993-01-01"),
                          ymd("2018-12-31")),
               date_labels = "%Y") +
    geom_hline(yintercept = 0) +
  theme_bw(base_size=7) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor = element_blank())) %>%
  ggsave(filename = "Figures/Middle_Loire/decomposition_gpp_discharge.png",
         device = "png",
         dpi = 300,
         height = 9.2,
         width = 18.4,
         units = "cm")

library(trend)
mk.test(trend_stl_gpp)
partial.cor.trend.test(trend_stl_gpp, trend_stl_q, method="pearson")
partial.cor.trend.test(trend_stl_gpp, trend_stl_q, method="spearman")

partial.mk.test(trend_stl_gpp, trend_stl_q)

# Discharge summer
q_ts <- df_q %>%
  dplyr::filter(between(date, ymd("1993-01-01"),
                        ymd("2018-12-31")),
                between(month(date), 4, 10),
                year(date) < 2004) %>%
  right_join(ungroup(df_met %>%
                       dplyr::filter(between(date, ymd("1993-01-01"),
                                             ymd("2018-12-31")),
                                     between(month(date), 4, 10),
                                     year(date) < 2004) %>%
                       dplyr::select(date))) %>%
  ungroup() %>%
  dplyr::select(discharge.daily) %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
q_ts <- ts(q_ts[,1], frequency = 214)
stl_q <- stl(q_ts, "periodic")
seasonal_stl_q   <- stl_q$time.series[,1]
trend_stl_q     <- stl_q$time.series[,2]
random_stl_q  <- stl_q$time.series[,3]

partial.cor.trend.test(trend_stl_gpp_sum, trend_stl_q, method="pearson")
partial.cor.trend.test(trend_stl_gpp_sum, trend_stl_q, method="spearman")

partial.mk.test(trend_stl_gpp_sum, trend_stl_q)


# simpler summer analysis
df_met_sum <- df_met %>%
  mutate(year = year(date),
         month = month(date),
         season = case_when(
           month %in% 9:11 ~ "Fall",
           month %in%  6:8  ~ "Summer",
           month %in%  3:5  ~ "Spring",
           TRUE ~ "Winter"),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER))
df_met_sum2 <- df_met_sum %>%
  group_by(year, season) %>%
  summarize(med_gpp = mean(GPP, na.rm = TRUE),
            med_er = mean(ER, na.rm = TRUE)) %>%
  filter(season == "Summer")
summary(lm(med_er ~ year, data = df_met_sum2))

(ggplot(data = filter(df_met_sum, season == "Summer"),
        aes(x = year,
            y = ER)) +
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
    stat_smooth(method = "lm", geom = "line"))







df_met_p <- df_met_sum %>%
  mutate(`P:R` = GPP / -ER) %>%
  pivot_longer(names_to = "flux",
               values_to = "value",
               cols = c(GPP, ER, `P:R`)) %>%
  group_by(year, month, flux) %>%
  summarize(med = median(value, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year,
                          month,
                          "01",
                          sep = "-")))
(ggplot(data = df_met_p,
       aes(x = date,
           y = med)) + 
  theme_bw() + geom_line() +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                            "5 years"),
               minor_breaks = seq(ymd("1993-01-01"), ymd("2020-01-01"),
                                  "1 years"),
               labels = date_format("%Y")) +
  facet_wrap(~flux, ncol = 1, scales = "free_y") + 
  theme(axis.title.x = element_blank()) +
  xlab("") +
  ylab(expression(atop("Flux", "(mg"~O[2]~m^2*d^{-1}*")")))) %>%
  ggsave(filename = "Figures/Middle_Loire/metab_monthly_median.png",
         device = "png",
         width = 6,
         height = 6,
         units = "in",
         dpi = 300)
  


# Discharge/GPP example plots ---------------------------------------------
# define limits to axes
ylim.prim <- c(0, 20)   
ylim.sec <- c(0, 2000)

# Calculate the plot variables for the axes
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

# year to plot
for(i in 1993:2018){
yr <- i
(ggplot() +
  geom_line(data = filter(df_met, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
            aes(x = date,
                y = GPP),
            color = "black") +
  geom_line(data = filter(df_q, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
            aes(x = date,
                y = a + discharge.daily * b),
            color = "blue") +
  scale_x_date(date_breaks = "1 month",
               limits = c(ymd(paste0(yr,"-01-01")),
                          ymd(paste0(yr,"-12-31"))),
               date_labels = "%m") +
  theme_bw(base_size=7) +
  theme(panel.grid.minor = element_blank(),
        axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     sec.axis = sec_axis(~ (. - a) / b, 
                                         name = expression("Mean daily discharge ("*m^3~s^{-1}*")")
                     )
  ) +
  ylab(expression(GPP~(g~O[2]~m^{-2}~d^{-1}))) +
  xlab("month") +
  ggtitle(label = yr)) %>%
  ggsave(filename = paste0("Figures/Middle_Loire/GPP_Q_", yr, ".png"),
         device = "png",
         dpi = 300,
         width = 9,
         height = 9,
         units = "cm")
}


df_solutes %>% 
  filter(year %in% c(1997, 2011, 2017), between(month, 5, 8),
         solute == "CHLA") %>%
  group_by(year) %>%
  summarize(mean = mean(month_mean))





# Granger causality ER GPP ------------------------------------------------
# Granger causality ER GPP
df_nopool <- df_met
library(lmtest)
x <- grangertest(df_nopool$GPP[486:637],df_nopool$ER[486:637], order = 2)
grangertest(df_nopool$ER[486:637],df_nopool$GPP[486:637], order = 3)
grangertest(diff(df_nopool$GPP[486:637]),diff(df_nopool$ER[486:637]), order = 2)
grangertest(diff(df_nopool$ER[486:637]),diff(df_nopool$GPP[486:637]), order = 2)
grangertest(diff(df_nopool$K600.daily[486:637]), diff(df_nopool$ER[486:637]), order = 1)

grangertest(diff(df_nopool$GPP[9252:9404]),diff(df_nopool$ER[9252:9404]), order = 2)
grangertest(diff(df_nopool$ER[9252:94047]),diff(df_nopool$GPP[9252:9404]), order = 2)
grangertest(diff(df_nopool$K600.daily[9252:9404]), diff(df_nopool$GPP[9252:9404]), order = 2)
plot(df_nopool$GPP[1:365])
plot(diff(df_nopool$K600.daily[1:365]))


df_met_n <- df_met %>%
  ungroup() %>%
  # dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER,
         year = year(date),
         month = month(date)) %>%
  # filter(between(month, 5, 8)) %>%
  group_by(year) %>%
  nest()
library(furrr)
# granger causality
df_gc <- df_met_n %>%
  mutate(gc_er_gpp = future_map(data, ~grangertest(diff(.$ER) ~ diff(.$GPP), 
                                                   order = 1))) %>%
  unnest(gc_er_gpp)

(ggplot(data = na.omit(df_gc),
       aes(x = year,
           y = `Pr(>F)`)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 7) +
  scale_x_continuous(breaks = seq(1994, 2018, 2)) +
  xlab("") +
  ylab(expression(ER[summer]*`~`*GPP[summer]~Granger~causality~pval))) %>%
  ggsave(filename = "Figures/Middle_Loire/granger_causality_er_gpp_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")

ccf(df_nopool$GPP[486:637],-df_nopool$ER[486:637], 
    lag.max = 3)
x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
         lag.max = 3)
pluck(x, "acf", 4)
# cross correlation
df_ccf <- df_met_n %>%
  mutate(data_trim = future_map(data, na.trim),
         data_fill = future_map(data_trim, na.interpolation),
         ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
                                                   lag.max = 3)),
         lag0 = future_map(ccf_ge, pluck, "acf", 4),
         lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
  unnest(lag0, lag1)

ggplot(data = df_ccf) +
  geom_point(aes(x = year, y = lag0)) +
  geom_point(aes(x = year, y = lag1), color = "red")

# CCM
library(multispatialCCM)
df_met_clean <- df_met %>%
  ungroup() %>%
  dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER,
         year = year(date),
         month = month(date))


Accm <- na.interpolation(na.trim(df_met_clean$GPP[(120+365*10):(267+365*10)]))
Bccm <- na.interpolation(na.trim(-df_met_clean$ER[(120+365*10):(267+365*10)]))

maxE<-5 #Maximum E to test
#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]<-SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
E_A<-2
E_B<-2
signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                               predsteplist=1:10)
signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                               predsteplist=1:10)
# Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
CCM_boot_A<-CCM_boot(Accm, Bccm, E_A, tau=1, iterations=25)
CCM_boot_B<-CCM_boot(Bccm, Accm, E_B, tau=1, iterations=25)
ccmtest(CCM_boot_A,CCM_boot_B)
#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2)



x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
         lag.max = 3)

df_ccf <- df_met_n %>%
  mutate(data_trim = future_map(data, na.trim),
         data_fill = future_map(data_trim, na.interpolation),
         ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
                                             lag.max = 3)),
         lag0 = future_map(ccf_ge, pluck, "acf", 4),
         lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
  unnest(lag0, lag1)

ggplot(data = df_ccf) +
  geom_point(aes(x = year, y = lag0)) +
  geom_point(aes(x = year, y = lag1), color = "red")

# Hysteresis plots --------------------------------------------------------
# summer average plot
(ggplot(df_mid_ss %>%
          filter(between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mp = mean(PO4, na.rm = TRUE),
                    mc = mean(CHLA, na.rm = TRUE),
                    sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
                    sec = sd(CHLA, na.rm = TRUE) / sqrt(n())), 
        aes(x = mp, y = mc, color = year)) + 
   geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
   geom_point() + 
   geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
   geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
   scale_color_viridis_c(name = "Year") +
   theme(legend.position = c(0.8, 0.4)) +
   scale_x_log10() + scale_y_log10() +
   ylab(expression("Mean summer chlorophyll-a (mg "~L^{-1}*")")) +
   xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))) %>%
  ggsave(filename = "Figures/Middle_Loire/chla_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)

# Turbidity vs nutrients
(ggplot(df_mid_ss %>%
          filter(between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mp = mean(PO4, na.rm = TRUE),
                    ms = mean(SPM, na.rm = TRUE),
                    sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
                    ses = sd(SPM, na.rm = TRUE) / sqrt(n())), 
        aes(x = mp, y = ms, color = year)) + 
    geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
    geom_point() + 
    geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
    geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.8, 0.4)) +
    scale_x_log10() + scale_y_log10() +
    ylab(expression("Mean summer TSS (mg "~L^{-1}*")")) +
    xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))) %>%
  ggsave(filename = "Figures/Middle_Loire/turb_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)




df_cam <- read_excel("Data/Camille_phd/high_freq_data.xlsx")
ggplot(df_cam %>%
         filter(between(month(Date), 4, 10),
                site == "cm"),
       aes(x = `P-PO43- (µg/L)`, y = `Chla (µg/L)`, color = year(Date))) + 
  geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  # scale_x_continuous(limits = c(0,2)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_mid_ss,
       aes(x = PO4, y = CHLA, color = year)) + 
  geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  # scale_x_continuous(limits = c(0,2)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_all %>%
         mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
         filter(solute == "CHLA",
                between(month, 6, 8)),
       aes(x = value, y = GPP, color = year)) + 
  # geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  scale_x_log10() 

ggplot(df_all %>%
         mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
         filter(solute == "CHLA",
                between(month, 5, 10)),
       aes(x = value, y = GPP, color = year)) + 
  # geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  stat_summary(fun.y = mean, geom = "point") +
  # stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  scale_color_viridis_c() +
  scale_x_log10() 

(ggplot(df_all  %>%
          mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
          filter(solute == "CHLA",
                 between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mgpp = mean(GPP, na.rm = TRUE),
                    mc = mean(value, na.rm = TRUE),
                    segpp = sd(GPP, na.rm = TRUE) / sqrt(n()),
                    sec = sd(value, na.rm = TRUE) / sqrt(n())), 
        aes(x = mc, y = mgpp, color = year)) + 
    geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
    geom_point() + 
    geom_errorbar(aes(ymin = mgpp - segpp, ymax = mgpp + segpp)) +
    geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.85, 0.25)) +
    scale_y_log10() +
    xlab(expression("Mean summer chlorophyll-a (mg "~L^{-1}*")")) +
    ylab(expression("Mean summer GPP"~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/gpp_chl_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)

(ggplot(df_all  %>%
          mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
          filter(solute == "PO4",
                 between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mgpp = mean(GPP, na.rm = TRUE),
                    mp = mean(value, na.rm = TRUE),
                    segpp = sd(GPP, na.rm = TRUE) / sqrt(n()),
                    sep = sd(value, na.rm = TRUE) / sqrt(n())), 
        aes(x = mp, y = mgpp, color = year)) + 
    geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
    geom_point() + 
    geom_errorbar(aes(ymin = mgpp - segpp, ymax = mgpp + segpp)) +
    geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.85, 0.25)) +
    scale_y_log10() +
    xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")")) +
    ylab(expression("Mean summer GPP"~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/gpp_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)


ggplot(df_solutes %>%
         filter(solute == "PO[4]^{`3-`}-P",
                between(month(date), 4, 10)) %>%
         mutate(dx = month_mean - lag(month_mean),
                dt = abs(month - lag(month)),
                dot = dx/dt), 
       aes(x = month_mean, y = dot, color = date)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0,0.5)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_solutes %>%
         filter(solute == "Chlorophyll~a",
                between(month(date), 4, 10)) %>%
         mutate(dx = month_mean - lag(month_mean),
                dt = abs(month - lag(month)),
                dot = dx/dt), 
       aes(x = month_mean, y = dot, color = date)) + 
  geom_point() 


# Corbicula filtration ----------------------------------------------------
df_cor_filt <- df_cor %>%
  filter(site == "dampierre") %>%
  mutate(filt_rate = 500 / 3600 / (1000^2), #m3/s per organism
         reach_vol = 10000 * 200 * 0.8, #m3
         Q = 100, #m3/s
         org_reach = density * 200 * 10000, #num of organisms in reach
         tot_filt = filt_rate * org_reach, # m3/s total filtration in reach
         turnover = (reach_vol / Q) / (reach_vol / tot_filt))  

