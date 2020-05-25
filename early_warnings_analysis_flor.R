# 
# Purpose: To do early warning analysis of Middle Loire data
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")

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

# Set seed
set.seed(42)

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
df_met_dt <- 
  read_csv2("Data/flor_data_test_granger.csv") %>%
  mutate(date = dmy(date),
         nep = gpp + er)
  # df_gp %>%
  # readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  # ungroup() %>%
  # mutate(GPP = ifelse(GPP < 0, NA, GPP),
  #        ER = ifelse(ER > 0, NA, ER),
  #        NEP = GPP + ER) #%>%
  # left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  # mutate(GPPq = GPP / log(discharge.daily),
  #        ERq = ER / log(discharge.daily),
  #        NEPq = NEP / log(discharge.daily))

# Short function for converting to time series
ts_conv_dt <- function(data){
  data = data %>%
    arrange(date) %>%
    # mutate(ind = row_number()) %>%
    na.trim() %>%
  #   as.data.frame(.)
  # dat_ts = data %>%
  #   na.trim(.) %>%
    as.data.frame(.) %>%
    na_kalman(.) %>%
    # na_seadec(.,find_frequency = TRUE, algorithm = "kalman") %>%
    xts(x = .[, "value"],
        order.by = .[, "date"])
  # dat_ts = na.trim(diff(diff(dat_ts), lag = 180))
}

# data = x %>%
#   arrange(date) %>%
#   # mutate(ind = row_number()) %>%
#   na.trim() %>%
#   as.data.frame(.)
# dat_ts = data %>%
#   na.trim(.) %>%
#   as.data.frame(.) %>%
#   na_kalman(., maxgap = 3) %>%
#   na_seadec(., find_frequency = TRUE, algorithm = "kalman")
# 
# plot(dat_ts$value[1:365])
# 
# 

# x <- diff(df_met$GPP)
# plot(x)
# y <- diff(x, lag = 90)
# plot(y)
# Combine and analyze water quality data
# df_ts <- df %>%
#   filter(solute %in% c("TP", "CHLA")) %>%
#   # filter(between(month(date), 4, 9)) %>%
#   group_by(solute) %>%
#   nest() %>%
#   mutate(ts_dat = future_map(data, ts_conv),
#          ew = future_map(ts_dat, generic_ews, winsize = 2))
#          # , ch = future_map(ts_dat, ~ch_ews(bind_cols(date = index(.), 
#          #                                                  value = .))))
#          # ,bds = future_map(ts_dat, ~bdstest_ews(bind_cols(date = index(.), 
#          #                                                 value = .),
#          #                                       ARMAoptim = FALSE,
#          #                                       ARMAorder = c(1,0))))
# 

# Analyze metabolism data, first interpolate NAs for smooth time series
df_ts_met_dt <- df_met_dt %>%
  mutate(year = year(date)) %>%
  # select(-K600.daily, -NPP) %>%
  pivot_longer(cols = c(nep, gpp, er), names_to = "flux") %>%
  # mutate(year = year(date)) %>%
  group_by(flux) %>%
  nest() %>%
  mutate(ts_dat = future_map(data, ts_conv_dt))

# Then filter out for summer period, and conduct early warnigns
df_ew_dt <- df_ts_met_dt %>%
  mutate(ts_dat_filt = future_map(ts_dat, ~.[.indexmon(.) %in% 
                                               c(4, 5, 6, 7, 8, 9),])) %>% #for some reason xts index is one month off
  mutate(
    # ar = future_map(ts_dat, acf, lag.max  = 1, plot = FALSE))
         # ew = future_map(ts_dat_filt, generic_ews, winsize = 12,
         #                 # detrending = "first-diff",
         #                 logtransform = FALSE))
          sens = future_map(ts_dat_filt, sensitivity_ews, indicator = "ar1",
                            winsizerange = c(12, 27),
                            incrwinsize = 3))
         # , bds = future_map(ts_dat, ~bdstest_ews(bind_cols(date = index(.), 
         #                                                 value = .),
         #                                       ARMAoptim = FALSE,
         #                                       ARMAorder = c(1,0))))

# ar_res <- df_ew %>% 
#   mutate(ar1 = future_map(ar,~ pluck(., 1, 2))) %>%
#   unnest(ar1)

# p_flor_annual <- ggplot(data = ar_res,
#        aes(x = year,
#            y = ar1,
#            color = flux)) +
#   geom_line() +
#   scale_color_manual(name = "",
#                      breaks = c("er", "gpp", "nep"),
#                      values = c("light blue", "dark blue","dark green"),
#                      labels = c("ER", "GPP", "NEP")) +
#   scale_x_continuous(breaks = seq(1993, 2018, 5))

ggsave(plot = p_flor_annual,
       filename = "Figures/Middle_Loire/earlywarnings_florentina_detrend_annual_window3yr.png",
       device = "png",
       dpi = 300,
       width = 183,
       height = 100,
       units = "mm")


summary(lm(ar1~year, data = filter(ar_res, flux == "NEP")))
# Rolling expanding regression
roll <- function(data, n = nrow(data)) {
  # rollapplyr(1:n, 1:n, function(ix) cor(data$y[ix],data$x[ix], method = "kendall"))
  rollapplyr(1:n, 1:n, function(ix) summary(lm(y ~ x, data, subset = ix))$coefficients[1,4])
}
a <- ar_res %>%
  group_by(flux) %>%
  mutate(roll = roll(data.frame(x = year, y = ar1))) %>%
  ungroup() %>%
  arrange(flux, year)

# g <- mutate(df_ts_met,
#             sens = future_map(ts_dat, sensitivity_ews))
# cpt.meanvar(pluck(df_ts_met, 4, 1, 2))
# filter(met_plot_data, flux == "GPPq", name == "ar1")
# plot(x = index(pluck(g, 5, 3)), y = pluck(g, 5, 3)$Ktauestind)
# cpt.meanvar(pluck(g, 5, 2)$Ktauestind)
# rownames(pluck(g, 5, 2))[129]
# 0.25+129*0.0025
# 0.2582165- 0.2554777
# (pluck(df_ts_met, 3, 1))[5482]
# cpt.meanvar(filter(met_plot_data, flux == "NEPq", name == "ar1")$value)
# filter(met_plot_data, flux == "NEPq", name == "ar1")[2020,]
.# function to get time indices related to datetimes
index_fun <- function(ts_data){
  ts_data = tibble(date = index(ts_data),
                   timeindex = row_number(date),
                   value = as.numeric(ts_data))
}

# Get a dataframe of timeindices for metabolism data
df_ind_dt <- df_ts_met_dt %>%
  transmute(pdat = future_map(ts_dat, index_fun)) %>%
  unnest(cols = c(pdat)) %>%
  select(-value)

# Plot data for early warning signals of GPP/ER/NEP
met_plot_data_dt <- df_ew_dt %>%
  # mutate(pdat = future_map(ts_dat_filt, index_fun)) %>%
  select(-data, -ts_dat, -ts_dat_filt) %>%
  unnest(cols = c(ew)) %>%
  pivot_longer(cols = -c(flux,  timeindex)) %>%
  left_join(df_ind,
            by = c("timeindex",
                   # "year",
                   "flux"))

a <- met_plot_data %>%
  group_by(flux) %>%
  mutate(roll = roll(data.frame(x = timeindex, y = ar1))) %>%
  ungroup() %>%
  arrange(flux, timeindex)


# Do the same and add CHLA/TP
df_ind_solutes <- df_ts %>%
  transmute(pdat = future_map(ts_dat, index_fun)) %>%
  unnest(cols = c(pdat)) %>%
  select(-value)
  
sol_plot_data <- df_ts %>%
  mutate(pdat = future_map(ts_dat, index_fun)) %>%
  select(-data, -ts_dat, -pdat) %>%
  unnest(cols = c(ew)) %>%
  pivot_longer(cols = -c(solute, timeindex)) %>%
  left_join(df_ind_solutes,
            by = c("timeindex",
                   "solute"))

# rename for plotting
plotnames_dt <- tibble(name = c("ar1", "acf1", "densratio", "kurt", "cv",
                             "returnrate", "sd", "sk"),
                    plotname = c("ar(1)", "acf(1)", "density ratio", "kurtosis", "cv",
                                 "return rate", "standard deviation", "skewness"))
left_join(met_plot_data_dt, plotnames_dt, by = "name") -> met_plot_data_dt
left_join(sol_plot_data, plotnames, by = "name") -> sol_plot_data
met_plot_data_dt$dt <- factor(format(met_plot_data_dt$date, format = '%d%b%Y'))

p_flor <- ggplot() + 
  geom_line(data = filter(met_plot_data_dt, 
                           !(name %in% c("returnrate", "acf1", "cv",
                                         "sd", "densratio"))),
            aes(x = date, y = value,
                color = flux)) +
    scale_color_manual(name = "",
                       breaks = c("er", "gpp", "nep"),
                       values = c("light blue", "dark blue","dark green"),
                       labels = c("ER", "GPP", "NEP")) +
    # scale_x_date(date_breaks = "3 years",
    #              # limits = c(ymd("2000-01-01"), ymd("2019-01-01")),
    #              date_labels = "%Y") + 
    # geom_rect(data = filter(bp_out_met, key == "GPP"),
    #           aes(xmin = lower,
    #               xmax = upper,
    #               ymin = -Inf,
    #               ymax = Inf),
    #           alpha = 0.4,
    #           fill = "dark blue") +
    # geom_rect(data = filter(bp_out, solute == "Chlorophyll~a"),
    #           aes(xmin = lower,
    #               xmax = upper,
    #               ymin = -Inf,
    #               ymax = Inf),
    #           alpha = 0.4,
    #           fill = "red") +
    # geom_line(data = filter(sol_plot_data, !(name %in% c("returnrate", "acf1")),
    #                         solute == "CHLA"),
    #           aes(x = date,
    #               y = value),
    #           color = "red") +
  theme(legend.position = c(0.12, 0.18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~plotname, scales = "free_y", strip.position = "left", nrow = 3) +
  ggtitle("detrended metabolism, rolling window = 3yrs")
p_flor

  ggsave(plot = p_flor,
         filename = "Figures/Middle_Loire/earlywarnings_florentina_detrend_3yr.png",
         device = "png",
         dpi = 300,
         width = 183,
         height = 100,
         units = "mm")


mutate(pdat = future_map2(data, ew, ~left_join(.x, .y) %>%
                            pivot_longer(cols = -c(date, timeindex, value))))


?ch_ews


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

?confint
# Rolling cross correlation
df_ccf <- tibble(date = index(pluck(df_ts_met,3,1)),
                 gpp = pluck(df_ts_met, 3, 1),
                 er = pluck(df_ts_met, 3, 2))

x <- df_ccf %>%
  mutate(cc = rollapply())

x <- rollapply(df_ccf, 30 ,function(x) ccf(x[,1],x[,2],
                                           lag.max = 1,
                                           
                                           ), by.column=FALSE)
y <- as.tibble(x)
pluck(y$acf,1,2)
plot(map_dbl(y$acf,~pluck(., 2)))
library(TTR)
library(tidyquant)
z <- df_ccf %>%
  filter(between(month(date), 5, 7)) %>%
  mutate(lag_gpp = diff(lag(gpp)),
         er_mag = diff(abs(er))) %>%
  tq_mutate_xy(x = lag_gpp,
               y = er_mag,
               mutate_fun = runCor,
               n = 30,
               use = "pairwise.complete.obs",
               col_rename = "roll_ccf") %>%
  group_by(year(date), month(date)) %>%
  summarize(meanroll = mean(roll_ccf, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rn = row_number())
plot(z$date, z$roll_ccf)
plot(z$rn, z$meanroll, "line")
dev.off()



