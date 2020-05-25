# 
# Purpose: To plot supplementary material plots
# Author: Jake Diamond
# Date: January 13, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)
library(patchwork)
library(ggpmisc)
library(ggsci)

# All solute data ---------------------------------------------------------
# Load raw data
df_solutes <- readRDS("Data/Loire_DO/middle_loire_wq")
# Clean for NAs
df_solutes_p <- df_solutes %>%
  filter_at(vars(value), all_vars(!is.na(.)))
# Data for plots with monthly averages (get rid of one weird data point for spm)
df_solutes_p <- df_solutes_p %>%
  dplyr::filter(!(solute == "SPM" & year == 1982)) %>%
  group_by(solute, year, month) %>%
  summarize(date = mean(date),
            month_mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1980) %>%
  mutate(summer = if_else(between(month, 4, 10), "summer", "winter"),
         month_mean = if_else(solute == "CHLA", month_mean / 1000, month_mean)) #for mg/L

# Plot all solute data
p_solutes_all <- ggplot(data = filter(df_solutes_p,
                                      solute %in% c("BOD5", 
                                                    "CHLA", 
                                                    "NO3",
                                                    "PO4",
                                                    "SPM"))) + 
  geom_line(aes(x = date,
                y = month_mean,
                group = 1,
                alpha = summer)) +
  facet_wrap(~solute, scales = "free_y",
             labeller = label_parsed,
             ncol = 1,
             strip.position="left") +
  scale_alpha_manual(name = "",
                     breaks = c("summer", "winter"),
                     values = c(1, 0.2)) +
  scale_color_viridis_d() +
  # scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
  #              breaks = brks,
  #              labels = labs_yrs) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        strip.placement = "outside",
        plot.margin=unit(c(0,0,0,0),"mm")) +
  xlab("") +
  ylab(expression("concentration (mg "~L^{-1}*")"))
p_solutes_all
ggsave(plot = p_solutes_all,
       filename = "Figures/Middle_Loire/supplementary/all_solute_data.tiff",
       device = "tiff",
       dpi = 300,
       width = 18.4,
       height = 9.2,
       units = "cm")


# Concentration discharges ----------------------------------
# Load discharge data and join to solutes
df_q <- readRDS("Data/dampierre_discharge_daily")
df_cq <- left_join(df_solutes, df_q) %>%
  distinct()
quantile(df_q$discharge.daily, 0.5, na.rm = T)
# Clean data and add rough periods based on breakpoints (2004)
df_cq <- df_cq %>%
  filter_at(vars(discharge.daily, value), all_vars(!is.na(.))) %>%
  mutate(period = if_else(year < 2004, 1, 2))

p_cq <- ggplot(data = filter(df_cq, year > 1990,
                             between(month,5,9),
                             discharge.daily < 216,
                             solute %in% c("PO4", "BOD5", "NO3")),
       aes(x = discharge.daily,
           y = value,
           color = as.factor(period))) + 
  geom_point(alpha = 0.7) +
  scale_y_log10() +
  scale_x_log10() +
  stat_smooth(method = "lm") +
  facet_wrap(~solute, scales = "free_y") + 
  scale_color_viridis_d(name = "period") +
  theme_bw() + 
  xlab("Discharge (m3/s)") +
  ylab("Parameter value")
p_cq
ggsave(p_cq,
       filename = "Figures/Middle_Loire/supplementary/solute_CQ.tiff",
       device = "tiff",
       dpi = 300,
       width = 18.4,
       height = 18.4,
       units = "cm")
# Metabolism supplementary ------------------------------------------------
# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")
# Get in long format and good names for plots
df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  gather(key, value, -date) %>%
  mutate(type_plot = recode(key,
                            `ER` = "ER~(g~O[2]~m^{-2}~d^{-1})",
                            `GPP` = "GPP~(g~O[2]~m^{-2}~d^{-1})",
                            `NEP` = "NEP~(g~O[2]~m^{-2}~d^{-1})",
                            `K600.daily` = "k[600]~(d^{-1})"))

df_met_l$type_plot <- factor(df_met_l$type_plot,
                             levels = c("GPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "ER~(g~O[2]~m^{-2}~d^{-1})",
                                        "NEP~(g~O[2]~m^{-2}~d^{-1})",
                                        "k[600]~(d^{-1})"))

# Plot the data
p_met_all <- ggplot(data = df_met_l,
        aes(x = date,
            y = value)) +
    geom_point(alpha = 0.4, size = 0.8) +
    facet_wrap(~type_plot, ncol = 1, scales = "free_y",
               labeller = label_parsed,
               strip.position = "left") + 
    scale_x_date(date_breaks = "1 years",
                 limits = c(ymd("1993-01-01"),
                            ymd("2018-12-31")),
                 date_labels = "%Y") +
    theme_bw(base_size=8) +
    theme(panel.grid.minor = element_blank(),
          strip.placement = "outside",
          strip.background = element_blank()) + 
    xlab("") +
    ylab("")
p_met_all
ggsave(plot = p_met_all,
       filename = "Figures/Middle_Loire/supplementary/metabolism_pool_facets.tiff",
       device = "tiff",
       dpi = 300,
       width = 18.4,
       height = 9.2,
       units = "cm")

# Plot of ER vs K600
p_er_k <- ggplot(data = df_met,
                 aes(x = K600.daily,
                     y = ER,
                     color = month(date))) +
  geom_point() +
  scale_y_continuous(limits = c(-25, 0)) +
  scale_color_viridis_c(name = "month", alpha = 0.5) +
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
  xlab(expression(k[600]~(d^{-1}))) +
  theme(legend.position = c(0.7, 0.12),
        legend.direction = "horizontal")

ggsave(plot = p_er_k,
       filename = "Figures/Middle_Loire/supplementary/ER_K.tiff",
       device = "tiff",
       dpi = 300,
       width = 9.2,
       height = 9.2,
       units = "cm")

# Summarize missing values
df_met %>% 
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise_all(~sum(is.na(.)))

# Plot of monthly changes
df_met %>%
  na_kalman() %>%
  mutate(year = year(date),
         month = month(date)) %>%
  select(-date) %>%
  group_by(year, month) %>%
  summarize_all(.funs = c(mean, median, sum)) %>%
  ggplot(aes(x = year,
             y = GPP_fn3)) +
  geom_point() + geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~month)

# Trend analysis ----------------------------------------------------------
# Time series analysis
gpp_ts <- df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  filter(year(date) > 1993) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_kalman() %>%
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
  dplyr::filter(between(date, ymd("1993-12-31"),
                        ymd("2018-12-31"))) %>%
  right_join(ungroup(df_met) %>%
               dplyr::select(date) %>%
               filter(year(date) > 1993)) %>%
  ungroup() %>%
  dplyr::select(discharge.daily) %>%
  na_kalman() %>%
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
-2.39e5/214


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

# Hysteresis plots --------------------------------------------------------
# Load all data
df_wq <- df_solutes %>%
  pivot_wider(names_from = solute, values_from = value)
df_combine <- left_join(df_met, df_wq, by = "date")
df_macros <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>%
  summarize(sa = sum(surface_area, na.rm = TRUE))
df_corb <- read_xlsx("Data/Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(density = mean(dens, na.rm = TRUE),
            se = sd(dens, na.rm = TRUE) / sqrt(n()))

df_lowflows <- readRDS("Data/Discharge/low_flow_duration")

# Summarize all data by year and join
df_all <- df_wq %>%
  filter(between(month, 4, 10),
         year > 1979) %>%
  group_by(year) %>%
  summarize(mp = mean(PO4, na.rm = TRUE),
            mc = mean(CHLA, na.rm = TRUE),
            ms = mean(SPM, na.rm = TRUE),
            ses = sd(SPM, na.rm = TRUE) / sqrt(n()),
            sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
            sec = sd(CHLA, na.rm = TRUE) / sqrt(n())) %>%
  left_join(df_macros) %>%
  left_join(df_corb) %>%
  left_join(df_met %>%
              mutate(year = year(date),
                     month = month(date)) %>%
              filter(between(month, 4, 10)) %>%
              group_by(year) %>%
              summarize(gpp = median(GPP, na.rm = TRUE),
                        seg = sd(GPP, na.rm = TRUE) / sqrt(n()))
  ) %>%
  left_join(df_lowflows) %>%
  mutate(veg = if_else(is.na(sa), "no_veg", "veg"),
         brk = if_else(year > 2005, "after", "before"),
         max_lf = max(lf_dur, na.rm = T)) 

# summer average plot for phosphate and chlorophyll
phos_chl_plot <- ggplot(df_all,
                        aes(x = mp, y = mc, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c(name = "Year") +
  theme(legend.position = c(0.85, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent"),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_chl_plot

# summer average plot for phosphate and turbidity
phos_tur_plot <- ggplot(df_all,
                        aes(x = mp, y = ms, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  # geom_line(data = df_mod, aes(x = x, y = y), linetype = "dashed") +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  theme(plot.tag.position = c(0.25,0.95)) +
  ylab(expression("Mean summer TSS (mg "~L^{-1}*")")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_tur_plot

# state space corbicula and po4
phos_cor_plot <- ggplot(df_all,
                        aes(x = density, y = mp, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = density - se, xmax = density + se)) +
  geom_errorbar(aes(ymin = mp - sep, ymax = mp + sep)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  theme(plot.tag.position = c(0.25,0.95)) +
  xlab(expression(italic(Corbicula )~sp.*" (ind "*m^{-2}*")")) +
  ylab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_cor_plot

# state space corbicula and chla
chl_cor_plot <- ggplot(df_all,
                       aes(x = density, y = mc, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = density - se, xmax = density + se)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent"),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  xlab(expression(italic(Corbicula )~sp.*" (ind "*m^{-2}*")")) +
  ylab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")"))
chl_cor_plot

# state space gpp and chla
chl_gpp_plot <- ggplot(filter(df_all, year >1992),
                       aes(x = mc, y = gpp, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  # geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
  # geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c(option = "cividis") +
  guides(
         fill = FALSE) +
  theme(legend.position = c(0.11, 0.7),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10() + 
  annotation_logticks(sides = "b") +
  ylab(expression("Median GPP ("*g~O[2]~m^{-2}~d^{-1}*")")) +
  xlab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")"))
chl_gpp_plot

# state space gpp and p
p_gpp_plot <- ggplot(filter(df_all, year >1992),
                     aes(x = mp, y = gpp, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  # geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
  # geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c(option = "cividis") +
  guides(color = FALSE,
         fill = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent"),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10() + 
  annotation_logticks(sides = "b") +
  ylab(expression("Median GPP ("*g~O[2]~m^{-2}~d^{-1}*")")) +
  xlab(expression("Mean summer phosphorus (mg"*~L^{-1}*")"))
p_gpp_plot

((phos_chl_plot | chl_cor_plot | chl_gpp_plot) / 
    (phos_tur_plot | phos_cor_plot | p_gpp_plot) +
    plot_annotation(tag_levels = "a")) %>%
  ggsave(filename = "Figures/Middle_Loire/supplementary/hysteresis_newest.tiff",
         device = "tiff",
         dpi = 300,
         height = 120,
         width = 183,
         units = "mm")
# Total Nitrogen ----------------------------------------------------------
# Total N over the years
(read_xlsx("Data/Loire_DO/Ntot.xlsx", sheet = 1) %>%
    select(year = Annee, TKN, NO3 = `N-NO3`) %>%
    pivot_longer(cols = c(TKN, NO3), names_to = "solute") %>%
    ggplot(data = .,
           aes(x = year,
               y = value,
               fill = solute)) +
    geom_col() +
    scale_fill_aaas() +
    scale_y_continuous(limits = c(0, 5)) +
    ylab("summer nitrogen (mg N/L)") +
    xlab("") +
    theme(legend.position = c(0.2, 0.8))) %>%
  ggsave(filename = "Figures/Middle_Loire/supplementary/summer_nitrogen_breakdown_bar_new.png",
         width = 10,
         height = 10,
         units = "cm",
         device = "png",
         dpi = 300)

# Hydrostatistics ---------------------------------------------------------
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


# Summer mean ER vs GPP
(df_met %>%
    mutate(month = month(date),
           year = year(date),
           GPP = ifelse(GPP < 0, NA, GPP),
           ER = ifelse(ER > 0, NA, ER),
           NPP = GPP + ER) %>%
    ungroup() %>%
    filter(between(month, 5, 9)) %>%
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
  ggsave(filename = "Figures/Middle_Loire/summer_ER_vs_GPP_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 9,
         units = "cm")


fingerprint <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 4, 10)) %>%
  ggplot(aes(x=GPP, y=ER) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha =0.3)  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white", alpha =0.3) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(-20, 0)) +
  theme(
    legend.position='none'
  )
x <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 5, 9)) %>%
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
         upper.ci.ER = ER_mean + qt(1 - (0.05 / 2), ER_n - 1) * ER_se)

(fingerprint + 
  geom_point(data = x, aes(x = GPP_mean,
                 y = ER_mean,
                 color = year)) +
  geom_text_repel(data = x, aes(x = GPP_mean,
                                y = ER_mean,
                                label = year,
                                color = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  scale_color_viridis_c(option = "magma") +
  geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "metab_fingerprint.png",
         dpi = 300,
         width = 6,
         height = 6,
         units = 'in')  
fingerprint + x



nep <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 4, 9)) %>%
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
  scale_color_viridis_c(option = "magma") +
  # geom_text_repel(aes(label = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  # geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  theme() +
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))


((fp / nep) +plot_layout(heights = c(3, 1),
                       widths = c(1, 1))) %>%
  ggsave(filename = "nep_witherrors.png",
         device = "png",
         dpi = 300,
         height = 6,
         width = 7.25,
         units = "in")















(df_met %>%
    ungroup() %>%
    # dplyr::select(-time_frame) %>%
    mutate(GPP = ifelse(GPP < 0, NA, GPP),
           ER = ifelse(ER > 0, NA, ER),
           NPP = GPP + ER,
           year = year(date),
           month = month(date)) %>%
    filter(between(month, 4, 10)) %>%
    ggplot() +
    theme_bw(base_size = 6) +
    theme(panel.grid.minor = element_blank()) +
    stat_summary(aes(x = year, y = GPP),
                 fun.y = mean, geom = "point") +
    # stat_summary(aes(x = year, y = GPP),
    #              fun.y = median, geom = "point", color = "red") +
    stat_summary(aes(x = year, y = GPP),
                 fun.data = mean_cl_boot, geom = "errorbar") + 
    # geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #             aes(x = year, y = GPP), method = "lm",
    #             alpha = 0.5) +
    # stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #              formula = y~x, 
    #              aes(x = year, y = GPP, 
    #                  label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #              parse = TRUE,
    #              eq.with.lhs = "italic(GPP[summer])~`=`~",
    #              label.x = 0.8, label.y = 0.55,
    #              size = 2) +
    stat_summary(aes(x = year, y = ER),
                 fun.y = mean, geom = "point") +
    # stat_summary(aes(x = year, y = ER),
    #              fun.y = median, geom = "point", color = "red") +
    stat_summary(aes(x = year, y = ER),
                 fun.data = mean_cl_boot, geom = "errorbar") +
    # geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #             aes(x = year, y = ER), method = "lm",
    #             alpha = 0.5) +
    # stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #              formula = y~x, 
    #              aes(x = year, y = ER, 
    #                  label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #              parse = TRUE,
    #              eq.with.lhs = "italic(ER[summer])~`=`~",
    #              label.x = 0.8, label.y = 0.02,
    #              size = 2) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(1994, 2018, 2)) +
    xlab("") +
    ylab(expression(flux~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/annual_flux_change_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")

ggplot(data = filter(df_mid_clean, solute == "CHLA"),
       aes(x = date, y = value)) + geom_point()





fp <- fingerprint + 
  geom_point(data = x, aes(x = GPP_mean,
                           y = ER_mean,
                           color = year)) +
  geom_text_repel(data = x, aes(x = GPP_mean,
                                y = ER_mean,
                                label = year,
                                color = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  scale_color_viridis_c(option = "magma") +
  geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1})))
