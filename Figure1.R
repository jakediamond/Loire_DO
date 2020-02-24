# 
# Purpose: To plot Figure 2, time series of biochemophysico params, for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(changepoint)
library(imputeTS)
library(tidyverse)

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm"))))

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
df_mid_wq <- filter(df, site_no %in% c("04048000",
                                       # "04048100",
                                       # "04049000",
                                       # "04049850",
                                       # "04050000",
                                       "04050500")) %>%
                                       # "04050550",
                                       # "04051000")) %>%
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
df_cor <- read_xlsx("Data//Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2)

# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Get into long format and prepare for plotting
df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  left_join(df_q) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  pivot_longer(cols = c(-date), names_to = "key", values_to = "value") %>%
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
  filter(species != "Elodea nuttallii") %>%
  add_tally(surface_area) %>%
  mutate(per = surface_area / n) %>%
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
# ggplot(filter(df_mid_wide, between(TP, 0, 0.1), 
#               PO4 != 0.05* 30.97 / 94.97,
#               year > 2008, 
#               site_no == "04048000", between(month, 4, 9)),
#        aes(x = TP,
#            y = PO4,
#            color = year,
#            shape = site_no)) + geom_point() +
#   stat_smooth(method = "lm")

lm_dat <- filter(df_mid_wide, between(TP, 0, 0.1), 
                 PO4 != 0.05* 30.97 / 94.97,
                 year > 2008, 
                 site_no == "04048000", between(month, 4, 9))
p_lm <- lm(lm_dat$PO4~lm_dat$TP)
# p_lm

# Same thing for BOD5 and CHLA
# ggplot(filter(df_mid_wide, 
#               !between(year, 2007, 2012),
#               site_no == "04048000", between(month, 4, 9)),
#        aes(x = CHLA,
#            y = BOD5,
#            color = year,
#            shape = site_no)) + geom_point() +
#   stat_smooth(method = "lm")

lm_dat2 <- filter(df_mid_wide, 
                  !between(year, 2007, 2012),
                  site_no == "04048000", between(month, 4, 9))
p_lm2 <- lm(lm_dat2$BOD5~lm_dat2$CHLA)
# p_lm2

# Estimate BOD5, PO4 for years 2008-2011
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

# # Quick look at data
# ggplot(filter(df_mid_clean, solute == "PO4")) +
#   geom_line(aes(x = date, y = value, color = site_no)) +
#   facet_wrap(~solute, scales = "free_y")
# 
# # Look at variability across sites before averaging
# df_mid_clean %>%
#   dplyr::filter(solute %in% c("BOD5", "CHLA", 
#                               "PO4", "NO3", "NP", "SPM",
#                               "TP", "PheoP", "Ppar"),
#                 between(month, 4, 9)) %>%
#   group_by(solute, year, month) %>%
#   summarize(date = mean(date),
#             month_mean = mean(value, na.rm = TRUE),
#             month_sd = sd(value, na.rm = TRUE),
#             month_cv = month_sd / month_mean) %>%
#   ggplot() +
#   geom_line(aes(x = date, y = month_cv)) +
#   facet_wrap(~solute, scales = "free_y")

# Data for plots with annual moving averages (get rid of one weird data point for spm)
df_solutes <- df_mid_clean %>%
  dplyr::filter(solute %in% c("BOD5", "CHLA", 
                              "PO4", "NO3", "SPM",
                              "TP"
                              # ,"NP",  "PheoP", "Ppar"
                              ),
                !(solute == "SPM" & year == 1982)
                ) %>%
  group_by(solute, year, month) %>%
  summarize(date = mean(date),
            month_mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(summer = if_else(between(month, 4, 9), "summer", "winter"))

# Calculate monthly running 90-day means of DO min and max
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
  
# Calculate annual values of corbicula
df_cor_sum <- df_cor %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(density = mean(dens, na.rm = TRUE),
            se = sd(dens, na.rm = TRUE) / sqrt(n())) %>%
  mutate(date = ymd(paste0(year, "-01-01")))

# Breakpoint analysis -----------------------------------------------------
# Estimate breakpoints
# Get nested solute data first
ts_dat <- df_mid_long %>%
  filter(year > 1979,
         solute %in% c("BOD5", "CHLA", "NO3", "TP", "PO4", "SPM")) %>%
  arrange(date) %>%
  group_by(year, month, solute) %>%
  distinct(date, .keep_all = TRUE) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(solute) %>%
  transmute(date = ymd(paste(year, month, "01", sep = "-")),
            value = mean) %>%
  # transmute(date = date,
  #           value = rmax12) %>%
  # filter(between(month(date), 6, 9)) %>%
  nest()

# Same for metabolism data
ts_met <- df_met_l %>%
  arrange(date) %>%
  group_by(key) %>%
  nest()

# Short function for converting to time series
ts_conv <- function(data, log = TRUE, type = "solute"){
  data = if(log){
    data %>%
      # filter(between(month(date), 4, 9)) %>%
      na.trim() %>%
      mutate(value = log(value)) %>%
      mutate(value = ifelse(is.nan(value), NA, value)) %>%
      as.data.frame(.)
  } else {
    data %>%
      # filter(between(month(date), 4, 9)) %>%
      na.trim() %>%
      mutate(value = ifelse(is.nan(value), NA, value)) %>%
      as.data.frame(.)
  }
  if(type == "solute"){
    dat_ts = xts(x = data[, -1],
                 order.by = data[, 1])
  } else{ 
    dat_ts = xts(x = data[, 2],
                 order.by = data[, 1])
  }
  dat_ts = na_interpolation(dat_ts, option = "stine")
  dat_ts = as.ts(dat_ts)
}

# Turn into time series and analyze
# chapts <- ts_dat %>%
#   mutate(ts = future_map(data, ts_conv),
#          cps = future_map(ts, cpt.meanvar),
#          cpt = future_map(cps, pluck, cpts),
#          ests = future_map(cps, pluck, param.est),
#          dates = future_map2(data, cpt, slice)) %>%
#   select(solute, dates, ests) %>%
#   hoist(dates,
#         brkdate = "date") %>%
#   ungroup() %>%
#   unnest_wider(ests) %>%
#   hoist(variance, 
#         variance1 = 1L,
#         variance2 = 2L) %>%
#   ungroup() %>%
#   hoist(mean,
#         mean1 = 1L,
#         mean2 = 2L) %>%
#   select(-dates)

# Turn into time series and analyze
library(mcp)
cla <- pluck(ts_dat, 2, 4) %>%
  mutate(ind = row_number(),
         cla = log(na_interpolation(value, option = "stine"))) %>%
  na.trim()
model = list(
  cla ~ 1 + ind + ar(1) + sigma(1),  # int_1, ar_1
  ~ 1 + ind + ar(1) + sigma(1) # time_2, ar1_2
)
fit_mcp = mcp(model, data = cla)

plot(fit_mcp)
summary(fit_mcp)
library(EnvCpt)
fit_envcpt = envcpt(na.trim(cla)$cla)
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est
dat <- ts_dat %>%
    mutate(ts = future_map(data, ts_conv),
           tsd = future_map(ts, as.vector)) %>%
  select(solute, tsd) %>% 
  unnest(tsd) %>%
  mutate(ind = row_number()) %>%
  ungroup()

model = list(
  tsd ~ (1|solute) + ar(1),  # int_1
  1 + (1|solute) ~ 0 + ind  # cp_1, cp_1_sd, cp_1_id[i]
)
fit = mcp(model, data = dat)
plot(fit, facet_by="solute", ncol = 3)
ranef(fit)
plot_pars(fit, pars = "varying", type = "trace", ncol=3)
# Get data ready for plot, only want to show the summer estimates
cpts_p <- chapts %>%
  transmute(solute = solute,
            brkyr = year(round_date(brkdate, "year"))) %>%
  left_join(df_solutes %>%
              filter(between(month, 4, 9))) %>%
  mutate(period = if_else(year < brkyr, 1, 2)) %>%
  group_by(solute, period, brkyr) %>%
  summarize(mean = mean(month_mean, na.rm = TRUE),
            sd = sd(month_mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = period,
              values_from = c(mean, sd)) %>%
  ungroup() %>%
  mutate(brkdate = ymd(paste0(brkyr, "-06-01")),
         yvals = c(14, 185, 4.2, 0.23, 135, 0.4))

# Changepoints for DO max and min
chapts_do <- df_do_rm %>%
  select(name, date, value) %>%
  group_by(name) %>%
  nest() %>%
  mutate(ts = future_map(data, ts_conv, log = FALSE),
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
        mean2 = 2L) %>%
  select(-dates) %>%
  transmute(name = name,
            brkyr = year(round_date(brkdate, "year"))) %>%
  left_join(df_do_rm) %>%
  mutate(period = if_else(year(date) < brkyr, 1, 2)) %>%
  filter(between(month(date), 4, 9)) %>%
  group_by(name, period, brkyr) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = period,
              values_from = c(mean, sd)) %>%
  ungroup() %>%
  mutate(brkdate = ymd(paste0(brkyr, "-06-01")),
         yvals = 200) %>%
  filter(name != "rm_min_wk")

# Changepoints for metabolism
chapts_met <- ts_met %>%
  mutate(ts = future_map(data, ts_conv, log = FALSE, type = "met"),
         cps = future_map(ts, cpt.meanvar),
         cpt = future_map(cps, pluck, cpts),
         ests = future_map(cps, pluck, param.est),
         dates = future_map2(data, cpt, slice)) %>%
  select(key, dates, ests) %>%
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

# Figure 2 plot -----------------------------------------------------------
# Chlorophyll a plot
# Reorder solutes and name for plotting
df_solutes$solute <- factor(df_solutes$solute,
                            levels = c("NO3",
                                       "TP",
                                       # "Ppar",
                                       "PO4",
                                       # "NP",
                                       "CHLA",
                                       "SPM",
                                       "BOD5"))
levels(df_solutes$solute) <- c("NO[3]^{`-`}-N",
                               "TP",
                               # "P[par]",
                               "PO[4]^{`3-`}-P",
                               # "N:P",
                               "Chlorophyll~a",
                               "TSS",
                               "BOD[5]")

# Rename changepoints for plotting
cpts_p$solute <- factor(cpts_p$solute,
                        levels = c("NO3",
                                   "TP",
                                   # "Ppar",
                                   "PO4",
                                   # "NP",
                                   "CHLA",
                                   "SPM",
                                   "BOD5"))
levels(cpts_p$solute) <- c("NO[3]^{`-`}-N",
                           "TP",
                           # "P[par]",
                           "PO[4]^{`3-`}-P",
                           # "N:P",
                           "Chlorophyll~a",
                           "TSS",
                           "BOD[5]")

p_chla <- ggplot(data = dplyr::filter(df_solutes,
                                      solute %in% c("Chlorophyll~a"))) + 
  geom_line(aes(x = date,
                y = month_mean,
                color = summer,
                group = 1)) +
  geom_vline(data = filter(cpts_p, solute %in% c("Chlorophyll~a")), 
             aes(xintercept = round_date(brkdate, "year")),
             linetype = "longdash",
             color = "dark green",
             size = 0.5,
             alpha = 0.8) +
  geom_text(data = filter(cpts_p, solute %in% c("Chlorophyll~a")), 
            aes(x = brkdate,
                y = yvals,
                label = brkyr),
            size = 2.2,
            color = "dark green") +
  geom_text(data = filter(cpts_p, solute %in% c("Chlorophyll~a")), 
            aes(x = ymd("1987-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[1])*",
                               "`]`==",
                               # "*",
                               round(mean_1, 2), 
                               "%+-%",
                              round(sd_1, 2),
                              ")")),
            parse = TRUE,
            size = 2.2,
            color = "dark green") +
  geom_text(data = filter(cpts_p, solute %in% c("Chlorophyll~a")),  
            aes(x = ymd("2014-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[2])*",
                               "`]`==",
                               # "*",
                               round(mean_2, 2), 
                               "%+-%",
                               round(sd_2, 2),
                               ")")),
            parse = TRUE,
            size = 2.2,
            color = "dark green") +
  scale_color_manual(name = "",
                     breaks = c("summer", "winter"),
                     values = c("dark green", "light green")) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  # guides(color = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.8, 0.6),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))
        ) +
  labs(subtitle = "A") +
  xlab("") +
  ylab(expression("Chlorophyll a ("*mu*g~L^{-1}*")"))
p_chla

# Macrophyte plot
p_mac <- ggplot(data = filter(df_mac, species != "Elodea nuttallii"),
                aes(x = year,
                    y = surface_area)) +
  # stat_summary(aes(fill = species2), fun.y = "sum", geom = "bar",
  #              color = "grey60") +
  geom_bar(aes(fill = species2), stat = "identity") +
  stat_summary(aes(linetype = type2), fun.y = "sum", geom = "line") + 
  scale_x_continuous(limits = c(1980, 2020),
                     breaks = seq(1980, 2020, 5)) +
  geom_vline(aes(xintercept = 2005),
             linetype = "longdash",
             color = "dark green",
             size = 0.5,
             alpha = 0.8) +
  scale_fill_brewer(name = "species", type = "qual", palette = "Accent") +
  scale_linetype_discrete(name = "macrophyte type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.22, 0.58),
        legend.key.height = unit(0.2, "cm"),
        legend.margin = margin(t = 0, b = -0.1, unit='cm')) +
  labs(subtitle = "B") +
  xlab("") +
  ylab(expression("Macrophyte surface area ( "*m^{2}*")"))
p_mac

# Plot corbicula data
p_c <- ggplot(data = df_cor_sum,
              aes(x = date,
                  y = density)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = density - se,
                    ymax = density + se)) +
  geom_vline(aes(xintercept = ymd("2005-01-01")),
             linetype = "longdash",
             color = "dark green",
             size = 0.5,
             alpha = 0.8) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"),
                            "5 years"),
               labels = date_format("%Y")) +
  scale_y_log10(sec.axis = sec_axis(~ . *200*10000*1.4e-7/100, 
                                         name = "Turnover ratio (-)")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.2, 0.6)) +
  labs(subtitle = "C") +
  xlab("") +
  ylab(expression(italic(Corbicula )~sp.~"(ind "*m^{-2}*")"))
p_c

# Plot DO data
p_do <- ggplot(data = df_do_rm,
               aes(x = date,
                   y = value * 100,
                   color = name)) + 
  geom_line(alpha = 0.8) +
  geom_vline(data = chapts_do, 
             aes(xintercept = brkdate),
             linetype = "longdash",
             color = "dark blue",
             size = 0.5,
             alpha = 0.8) +
  geom_text(data = chapts_do, 
            aes(x = brkdate,
                y = yvals,
                label = brkyr),
            size = 2.2,
            show.legend = FALSE) +
  geom_text(data = chapts_do, 
            aes(x = ymd("1987-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[1])*",
                               "`]`==",
                               # "*",
                               round(mean_1*100, 0), 
                               "%+-%",
                               round(sd_1*100, 0),
                               ")")),
            parse = TRUE,
            size = 2.2,
            show.legend = FALSE) +
  geom_text(data = chapts_do,  
            aes(x = ymd("2014-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[2])*",
                               "`]`==",
                               # "*",
                               round(mean_2*100, 0), 
                               "%+-%",
                               round(sd_2*100, 0),
                               ")")),
            parse = TRUE,
            size = 2.2,
            show.legend = FALSE) +
  scale_color_manual(name = "90-d running mean",
                        breaks = c("rm_max_wk", "rm_min_wk"),
                        labels = c("daily max", "daily min"),
                        values = c("dark blue", "light blue")) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
                   breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"),
                                "5 years"),
                   labels = date_format("%Y")) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.15, 0.7),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))) +
  labs(subtitle = "D") +
  xlab("") +
  ylab(expression("DO saturation (%)"))
p_do

# Add breakpoints to solutes
# Plot of monthly solute time series
p_solutes <- ggplot(data = filter(df_solutes,
                                  # between(month, 4, 10),
                                  solute %in% c("NO[3]^{`-`}-N",
                                                "TP",
                                                "PO[4]^{`3-`}-P",
                                                "TSS",
                                                "BOD[5]"))) + 
  geom_line(aes(x = date,
                y = month_mean,
                color = summer,
                group = 1)) +
  geom_vline(data = filter(cpts_p, solute %in% c("NO[3]^{`-`}-N",
                                                 "TP",
                                                 "PO[4]^{`3-`}-P",
                                                 "TSS",
                                                 "BOD[5]")), 
             aes(xintercept = round_date(brkdate, "year")),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8) +
  geom_text(data = filter(cpts_p, solute %in% c("NO[3]^{`-`}-N",
                                                 "TP",
                                                 "PO[4]^{`3-`}-P",
                                                 "TSS",
                                                 "BOD[5]")), 
            aes(x = brkdate,
                y = yvals,
                label = brkyr),
            size = 2.2) +
  geom_text(data = filter(cpts_p, solute %in% c("NO[3]^{`-`}-N",
                                                 "TP",
                                                 "PO[4]^{`3-`}-P",
                                                 "TSS",
                                                 "BOD[5]")), 
            aes(x = ymd("1987-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[1])*",
                               "`]`==",
                               # "*",
                               round(mean_1, 2), 
                               "%+-%",
                               round(sd_1, 2),
                               ")")),
            parse = TRUE,
            size = 2.2) +
  geom_text(data = filter(cpts_p, solute %in% c("NO[3]^{`-`}-N",
                                                 "TP",
                                                 "PO[4]^{`3-`}-P",
                                                 "TSS",
                                                 "BOD[5]")),  
            aes(x = ymd("2014-01-01"),
                y = yvals,
                label = paste0("list(","`[`*",
                               "bar(summer[2])*",
                               "`]`==",
                               # "*",
                               round(mean_2, 2), 
                               "%+-%",
                               round(sd_2, 2),
                               ")")),
            parse = TRUE,
            size = 2.2) +
  facet_wrap(~solute, scales = "free_y",
             labeller = label_parsed,
             ncol = 1) +
  scale_color_manual(name = "",
                     breaks = c("summer", "winter"),
                     values = c("black", "grey60")) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), 
                            "5 years"),
               labels = date_format("%Y")) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.88, 0.72),
        legend.key.height = unit(0.2, "cm")
        ) +
  labs(subtitle = "E") +
  xlab("") +
  ylab(expression("Concentration (mg "~L^{-1}*")"))
p_solutes

# Plot of metabolism data
p_met <- ggplot(data = filter(df_met_l, key %in% c("GPP", "ER")),
               aes(x = date,
                   y = value)) + 
  geom_point(aes(color = key), alpha = 0.3, size = 0.15) +
  # geom_line(data = gpp_trend,
  #           aes(x = date,
  #               y = trend,
  #               linetype = "trend")) +
  scale_color_manual(name = "",
                     # labels = parse_format(),
                     values = c("light blue", "dark blue")) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"),
                            "5 years"),
               labels = date_format("%Y")) +
  geom_hline(yintercept = 0) +
  geom_vline(data = chapts_do, 
             aes(xintercept = brkdate),
             linetype = "longdash",
             color = "dark blue",
             size = 0.5,
             alpha = 0.8) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.15, 0.15),
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))) +
  labs(subtitle = "E") +
  xlab("") +
  ylab(expression("Metabolic flux"~(g~O[2]~m^{-2}~d^{-1})))
p_met

nep <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  # filter(between(month, 4, 9)) %>%
  group_by(year) %>%
  summarize(NEP_mean = mean(NEP, na.rm = TRUE),
            NEP_sd = sd(GPP, na.rm = TRUE),
            NEP_n= n()) %>%
  mutate(NEP_se = NEP_sd / sqrt(NEP_n),
         lower.ci.NEP = NEP_mean - qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se,
         upper.ci.NEP = NEP_mean + qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se) %>%
  ggplot(aes(x = year,
             y = NEP_mean,
             color = year)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower.ci.NEP, ymax = upper.ci.NEP), width =0.2)  +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(1980, 2020, 5),
                     limits = c(1980, 2020)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * -60000*200*44*365*12/32/44/1e9, 
                                         name = "Annual C efflux to atmosphere (Gg C)")) +
  scale_color_viridis_c(name = "year") +
  theme(legend.key.height = unit(0.2, "cm"),
        legend.direction = "horizontal",
        legend.position = c(0.5, 0.85),
        legend.background = element_rect(color = "transparent")) +
  labs(subtitle = "F") +
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))

# Plot all plots
(p_chla / p_mac / p_c / p_met / nep) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure1.tiff",
         device = "tiff",
         dpi = 300,
         height = 200,
         width = 183,
         units = "mm")
