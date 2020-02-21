# 
# Purpose: To do time series analysis of GPP data
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

# Load data
# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Time series analysis
# First get rid of NAs with stine interpolation
df_met_clean <- df_met %>%
  select(-NPP) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  na.trim(.) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()

# Get data into time series format
gpp_ts <- ts(df_met_clean$GPP, frequency = 365)

# Seasonal decomposition by LOESS
stl_gpp <- stl(gpp_ts, "periodic")
seasonal_stl_gpp   <- stl_gpp$time.series[,1]
trend_stl_gpp     <- stl_gpp$time.series[,2]
random_stl_gpp  <- stl_gpp$time.series[,3]

plot(gpp_ts)
plot(as.ts(seasonal_stl_gpp))
plot(trend_stl_gpp)
plot(random_stl_gpp)
plot(stl_gpp)

summary(lm(trend_stl_gpp ~ index(seasonal_stl_gpp)))
gpp_trend <- as.tibble(pluck(stl_gpp, "time.series")) %>%
  mutate(type = "GPP",
         time = dplyr::row_number(),
         seasonal = scale(seasonal),
         remainder_scale = scale(remainder),
         trend_scale = scale(trend)) %>%
  left_join(mutate(df_met_clean, time = dplyr::row_number()) %>%
              dplyr::select(time, date))

# Discharge
q_ts <- df_q %>%
  dplyr::filter(between(date, ymd("1993-01-01"),
                        ymd("2018-12-31"))) %>%
  right_join(ungroup(df_met_clean) %>%
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
  left_join(mutate(df_met_clean, time = dplyr::row_number()) %>%
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


