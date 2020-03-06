# 
# Purpose: To do wavelet analysis on time series data for middle Loire
# Author: Jake Diamond
# Date: February 12, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(furrr)
library(hydrostats)
library(patchwork)
library(xts)
library(imputeTS)
library(WaveletComp)
library(tidyverse)


df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")
# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")
# Some cleaning
df_use <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  left_join(df_q) %>%
  filter(between(month, 6, 8)) %>%
  na.trim()

ts_conv <- function(data){
  data = data %>%
    na.trim() %>%
    as.data.frame(.)
  dat_ts = xts(x = data[, -1],
               order.by = data[, 1])
  dat_ts = na_interpolation(dat_ts, option = "stine")
  dat_ts = as.data.frame(dat_ts)
}
q <- ts_conv(select(df_use, date, discharge.daily))
gpp <- ts_conv(select(df_use, date, GPP))
er <- ts_conv(select(df_use, date, ER))
myw <- analyze.wavelet(gpp, 1,
                       loess.span = 0,
                       dt = 1, dj = 1/250,
                       lowerPeriod = 16,
                       upperPeriod = 128,
                       make.pval = TRUE, n.sim = 10)
wt.image(myw, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
reconstruct(myw, plot.waves = FALSE, lwd = c(1,2),
            legend.coords = "bottomleft")


my.data <- data.frame(x = gpp, y = er)
my.wc <- analyze.coherency(my.data, my.pair = c(1,2),
                           # loess.span = 0,
                           dt = 1/365, dj = 1/20,
                           lowerPeriod = 4/365,
                           upperPeriod = 365/365,
                           make.pval = TRUE, n.sim = 10,
                           date.format = "%Y-%m-%d")
wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (years)",
         show.date = TRUE,
         date.format = "%Y-%m-%d")
wc.avg(my.wc, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (yars)")

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "wavelet coherence levels"),
         timelab = "")


df_wq <- readRDS("Data/Loire_DO/middle_loire_wq")

df <- df_wq %>%
  filter(year > 1979,
         solute %in% c("CHLA", "SPM")) %>%
  arrange(date) %>%
  group_by(year, month, solute) %>%
  distinct(date, .keep_all = TRUE) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(solute) %>%
  transmute(date = ymd(paste(year, month, "01", sep = "-")),
            value = mean) %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  filter(between(date, ymd("1980-07-01"), ymd("2018-10-01")))

p <- ts_conv(select(df, date, PO4))
chla <- ts_conv(select(df, date, CHLA))

my.data <- data.frame(x = p, y = chla)
my.wc <- analyze.coherency(my.data, my.pair = c(1,2),
                           loess.span = 0,
                           dt = 1/12, dj = 1/20,
                           lowerPeriod = 1/3,
                           make.pval = TRUE, n.sim = 100,
                           date.format = "%Y-%m-%d")
wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (years)",
         show.date = TRUE,
         date.format = "%Y-%m-%d")
