# 
# Purpose: To do early warning analysis of Middle Loire data
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
library(earlywarnings)

# Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load middle loire water quality data
df <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  ungroup() %>%
  group_by(solute, year, month) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  filter(year > 1979) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  select(solute, date, value)

# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER)

# Short function for converting to time series
ts_conv <- function(data){
  data = data %>%
    arrange(date) %>%
    na.trim() %>%
    as.data.frame(.)
  dat_ts = xts(x = data[, "value"],
               order.by = data[, "date"])
  dat_ts = na_interpolation(dat_ts, option = "stine")
  dat_ts = as.ts(dat_ts)
}

# Combine data
df_ts <- df %>%
  filter(solute %in% c("TP", "CHLA")) %>%
  group_by(solute) %>%
  nest() %>%
  mutate(ts_dat = future_map(data, ts_conv),
         ew = future_map(ts_dat, generic_ews, winsize = 20, 
                         detrending = "first-diff",
                         logtransform = TRUE),
         bds = future_map(ts_dat, ~bdstest_ews(bind_cols(date = index(.), 
                                                         value = .),
                                               ARMAoptim = FALSE,
                                               ARMAorder = c(1,0))))

x<-diff(pluck(df_ts,3,2))
plot(x)
pacf(x)
acf(x)
bds.test(x)
arma(x)
ar <- arima(x, order = c(1,0,0))
pacf(ar$residuals)
dev.off()
y <- as.matrix(bind_cols(date = index(x), 
              value =coredata(x)))
bdstest_ews(y, ARMAoptim = FALSE)
plot(pluck(df_ts,4,1)$timeindex, pluck(df_ts,4,1)$sk)
summary(ew)
ew
ch_ew <- ch_ews(p_ts)
ch_ew
ch_ew$cusum <- cumsum(ch_ew$test.result)
plot(ch_ew$time, ch_ew$cusum)
fs_pa <- Fstats(p_ts ~ 1)
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
ew <- generic_ews(chla_ts[1:bp$breakpoints],
                  winsize = 10, 
                  # detrending = "first-diff",
                  logtransform = FALSE)






# Turn into time series and analyze
bps <- ts_dat %>%
  mutate(ts = future_map(data, ts_conv),
         cps = future_map(ts, ~breakpoints(.~1)),
         confints = future_map(cps, confint)) %>%
  unnest(cols = confints)
pluck(bps, 5,4)
x <-pluck(bps, 2,4)
,
cpt = future_map(cps, pluck, cpts),
ests = future_map(cps, pluck, param.est),
dates = future_map2(data, cpt, slice)) %>%
  select(solute, dates, ests)