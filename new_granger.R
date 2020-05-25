# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------
library(tidyverse)
library(lubridate)
library(imputeTS)
library(readxl)
library(vars)
library(furrr)
df <- read_excel("Data/all_corrected_rawdata_models for detrend_residuals.xlsx", 
                 sheet = 8) %>%
  dplyr::select(-max_DO, -min_DO, -Amplitude, -LogQ, - NEP)
  # select(date, GPP = GPP_Res, ER = ER_res, chla = Chla_Res)
df <- df[-9486,]
df$gpp_rate <- log(df$GPP / lag(df$GPP))
df$er_rate <- log(df$ER / lag(df$ER))
df$gpp_diff <- c(NA, diff(df$GPP))
df$er_diff <- c(NA, diff(df$ER))
df <- na_kalman(df)
plot(df$gpp_rate)

gc_fun <- function(data, detrend = "rate"){
  if(detrend == "rate"){
    ts = data[1:nrow(data) - 1, c("gpp_rate", "er_rate")]
    var = VAR(ts, p = 1, type = "const")
    gc_res_gpp = causality(var, cause = "gpp_rate")$Granger
    gc_res_er = causality(var, cause = "er_rate")$Granger
  }else if(detrend == "firstdiff"){
    ts = data[1:nrow(data) - 1, c("gpp_diff", "er_diff")]
    var = VAR(ts, p = 1, type = "const")
    gc_res_gpp = causality(var, cause = "gpp_diff")$Granger
    gc_res_er = causality(var, cause = "er_diff")$Granger
  }else{
    ts = data[1:nrow(data) - 1, c("GPP", "ER")]
    var = VAR(ts, p = 1, type = "const")
    gc_res_gpp = causality(var, cause = "GPP")$Granger
    gc_res_er = causality(var, cause = "ER")$Granger
  }
  results = data.frame(causality = c("GPP causes ER",
                                     "ER causes GPP"),
                       pval = c(gc_res_gpp$p.value,
                                gc_res_er$p.value)
                       )
}

gc_res <- df %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(between(month, 5, 9)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(gc = future_map(data, gc_fun, detrend = "none")) %>%
  unnest(gc)

ggplot(data = filter(gc_res, causality == "GPP causes ER"),
       aes(x = year,
           y = pval,
           color = causality)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed")







# For use in paper

gc_fun <- function(data, detrend = "rate", lag = NULL){
  if(detrend == "rate"){
    data$gpp_rate = log(data$GPP / lag(data$GPP)) #make stationary
    data$er_rate = log(data$ER / lag(data$ER)) #make stationary
    data = na_kalman(data) #fill nas with kalman filter
    # test for stationarity
    # p_stat_gpp = adf.test(data$gpp_rate)$p.val
    # p_stat_er = adf.test(data$er_rate)$p.val
    # if(p_stat_gpp > 0.05) warning('GPP data nonstationary')
    # if(p_stat_er > 0.05) warning('ER data nonstationary')
    ts = data[1:nrow(data) - 1, c("gpp_rate", "er_rate")]
    var = VAR(ts, p = 1, type = "const", lag.max = lag) #vector autoregression
    gc_res_gpp = causality(var, cause = "gpp_rate")$Granger
    gc_res_er = causality(var, cause = "er_rate")$Granger
  }else if(detrend == "firstdiff"){
    data$gpp_diff <- c(NA, diff(data$GPP))
    data$er_diff <- c(NA, diff(data$ER))
    data = na_kalman(data)
    # test for stationarity
    p_stat_gpp = adf.test(data$gpp_diff)$p.val
    p_stat_er = adf.test(data$er_diff)$p.val
    if(p_stat_gpp > 0.05) warning('GPP data nonstationary')
    if(p_stat_er > 0.05) warning('ER data nonstationary')
    ts = data[1:nrow(data) - 1, c("gpp_diff", "er_diff")]
    var = VAR(ts, p = 1, type = "const", lag.max = lag)
    gc_res_gpp = causality(var, cause = "gpp_diff")$Granger
    gc_res_er = causality(var, cause = "er_diff")$Granger
  }else{
    data = na_kalman(data)
    # test for stationarity
    p_stat_gpp = adf.test(data$GPP)$p.val
    p_stat_er = adf.test(data$ER)$p.val
    if(p_stat_gpp > 0.05) warning('GPP data nonstationary')
    if(p_stat_er > 0.05) warning('ER data nonstationary')
    ts = data[1:nrow(data) - 1, c("GPP", "ER")]
    var = VAR(ts, p = 1, type = "const", lag.max = lag)
    gc_res_gpp = causality(var, cause = "GPP")$Granger
    gc_res_er = causality(var, cause = "ER")$Granger
  }
  results = data.frame(causality = c("GPP causes ER",
                                     "ER causes GPP"),
                       pval = c(gc_res_gpp$p.value,
                                gc_res_er$p.value)
  )
}

gc_res <- df %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(between(month, 5, 9)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(gc = future_map(data, gc_fun, detrend = "firstdiff", lag = NULL)) %>%
  unnest(gc)

ggplot(data = filter(gc_res, causality == "GPP causes ER"),
       aes(x = year,
           y = pval,
           color = causality)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed")