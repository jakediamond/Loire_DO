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
library(patchwork)
library(xts)
library(viridis)
library(zoo)
library(readxl)
library(changepoint)
library(imputeTS)
library(tidyverse)

# Set the plotting theme
theme_set(theme_bw(base_size=6)+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

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
df_mid <- filter(df, site_no %in% c("04048000",
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
df_met %>%
  filter(between(date, ymd("1999-01-01"), ymd("1999-12-31"))) %>%
  ggplot(aes(x = date, y = GPP)) + geom_point()
plot(df_met$date, df_met$GPP)
# Load corbicula data
df_cor <- read_xlsx("Data/Loire_DO/corbicula.xlsx") %>%
  mutate(site = str_to_lower(site))

# Load discharge data
df_q <- readRDS("Data/dampierre_discharge_daily") %>%
  mutate(mon = month(date),
         mon2 = ifelse(mon > 9, mon%%10 + 1, mon + 3))

# Data analysis and seasonality -----------------------------------------------------------
# Some data summaries. Change all negatives to NA
df_mid <- df_mid %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  mutate(NP = NO3 / PO4 * (31/14)) %>%
  gather(solute, value, temp:NP)

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

# Time series plots
# Data for plots with moving averages
df_solutes <- df_mid %>%
  dplyr::filter(solute %in% c("BOD5", "CHLA", 
                              "PO4", "NO3", "NP", "SPM")) %>%
  group_by(solute, year, month) %>%
  summarize(month_mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")),
         period = ifelse(year < 2004, 1, 2)) %>%
  group_by(solute, period) %>%
  mutate(rm12 = rollapply(month_mean, width=12, 
                          FUN=function(x)
                            mean(x, na.rm=TRUE), by=1, 
                          by.column=TRUE, partial=TRUE, 
                          fill=NA, align="center"),
         month2 = ifelse(month > 9, month%%10 + 1, month + 3)
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
  group_by() %>%
  mutate(
    season = case_when(
      month %in% 9:11 ~ "Fall",
      month %in%  6:8  ~ "Summer",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Winter")) %>%
  group_by(period, solute, season) %>%
  summarize(seas_mn = mean(month_mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = season, values_from = seas_mn)

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


# Figure 2 plot -----------------------------------------------------------
# Add breakpoints to solutes
# Plot of monthly solute time series
p_solutes <- ggplot(data = df_solutes) + 
  geom_line(aes(x = date,
                y = month_mean,
                color = month2),
            alpha = 0.8) +
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
  scale_colour_viridis(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c(10,11,12,1,2,3,4,5,6,7,8,9)) + 
  guides(color = FALSE) +
  theme(axis.title.x = element_blank()) +
  labs(subtitle = "C") +
  xlab("") +
  ylab(expression("Concentration (mg "*L^{-1}*")"))
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
                  y = discharge.daily,
                  color = mon2)) + 
  geom_line() +
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
        legend.position = c(0.8, 0.85),
        legend.background = element_rect(fill=alpha("white", 0.4))) +
  labs(subtitle = "B") +
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
  theme(axis.title.x = element_blank()) +
  labs(subtitle = "D") +
  xlab("") +
  ylab(expression("ln("*italic(Corbicula~sp.)~"density + 1)"))
p_c

# Plot all plots
((p_do / p_q) | (p_solutes / (p_c)) * plot_layout(heights = c(3, 1))) %>%
  ggsave(filename = "Figures/Figure2.png",
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
                   color = year)) +
    scale_y_log10() +
    scale_x_log10() +
    facet_wrap(~solute, scales = "free_y") + 
    scale_color_viridis_c() +
    theme_bw() + 
    xlab("Discharge (m3/s)") +
    ylab("Parameter value")) %>%
  ggsave(.,
         filename = "Figures/To share/Dampierre_solute_CQ_all_sites.tiff",
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
df_all <- left_join(df_mid_clean, df_met, by = "date") %>%
  left_join(df_do %>%
              filter(site == "dampierre") %>%
              mutate(date = date(datetime)) %>%
              group_by(date) %>%
              summarize(max = max(DO_use, na.rm = TRUE),
                        amp = diff(range(DO_use, na.rm = TRUE)))
  )


ggplot(df_all) + geom_point(aes(x = value,
                                y = ER)) +
  facet_wrap(~solute, scales = "free")


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
    stat_poly_eq(formula = y~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE,
                 label.x = 0.8, label.y = 0.1) +
    theme_bw() +
    xlab("Median Chlorophyll a (mg/L)") +
    ylab("Summertime median GPP (g O2 m-2 d-1)")
) %>%
  ggsave(filename = "Figures/chla_vs_gpp.tiff",
         device = 'tiff',
         dpi = 400)
plot(log(df$TP), log(df$CHLA))

# GPP vs ER
# with quant
library(quantreg)
?rq
df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER)) %>%
  filter(between(month(date),6,9)) %>%
  ggplot(aes(x = lag(GPP),
             y = ER)) +
  geom_point() +
  facet_wrap(~year(date)) +
  theme_bw() +
  stat_smooth(method = "lm") +
  geom_quantile(quantiles = 0.9) +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label..)),
               parse = TRUE,
               label.x = 0.8, label.y = 0.1)








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
d <- ts.intersect(y = tp_ts, y1 = stats::lag(tp_ts, -1))
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


chla_ts <- df_mid %>%
  filter(solute == "CHLA") %>%
  group_by(solute, year, month) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  as.data.frame()

chla_ts = xts(x = chla_ts[, "mean"],
             order.by = chla_ts[, "date"])
chla_ts = na_interpolation(chla_ts, option = "stine")
chla_ts = as.ts(chla_ts)

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
plot(chla_ts)
lines(bp_ci)
coef(bp_chla)
chla_ts_dat$date[bp_chla$breakpoints]

pacf((window(chla_ts, end = 368)))
pacf((window(chla_ts, start = 368)))

d <- ts.intersect(y = log(chla_ts), y1 = stats::lag(log(chla_ts), -1))
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