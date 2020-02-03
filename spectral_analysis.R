# 
# Purpose: To look at spectral signatures of DO in EDF time series
# Author: Jake Diamond
# Date: November 8, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidyverse)
library(tseries)
library(readxl)

# Load DO data
df <- readRDS("Data/all_DO_cleaned")

# Load Q data
# # Generate daily time series
dat_seq <- data.frame(date = seq(ymd("1993-01-01"), 
                                 ymd('2018-12-31'), 
                                 by = "days"))
# Load discharge data, but this is missing 1994-1995
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  rename(discharge.daily = Qm3s) %>%
  select(discharge.daily, date)

# Load 1994 and 1995 data, but need to make average daily to match
df_q <- read_xls("Data/Moatar_thesis/DAM95AMC.XLS",
                 sheet = 1) %>%
  bind_rows(read_xls("Data/Moatar_thesis/DAM94AMC.XLS",
                     sheet = 4)) %>%
  select(datetime = DATE, discharge = DEB) %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(discharge.daily = mean(discharge, na.rm = TRUE)) %>%
  # drop_na() %>%
  bind_rows(df_q) %>%
  arrange(date) %>%
  right_join(dat_seq) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2018-12-31"))
  )

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)
df_q$period <- ifelse(year(df_q$date) < 2001, 1, 2)

# Plot raw  min max data, but filtered to 7 days
df_sum <- df %>%
  filter(site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date, period) %>%
  summarize(min = min(DO_use),
            max = max(DO_use),
            amp = diff(range(DO_use))) %>%
  pivot_longer(-c(period, date), names_to = "type") %>%
  ungroup()

p_min_max <-
  ggplot(filter(df_sum, type != "amp")) +
  geom_smooth(aes(x = date,
                 y = value,
                color = type),
              method = "loess",
              span = 0.008) +
  facet_wrap(~period, scales = "free_x") +
  scale_color_manual(name = "", values = c("black", "grey")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  xlab("") +
  ylab("DO (mg/L)")
p_min_max
ggsave(plot = p_min_max,
       filename = "Figures/dampierre_min_max_timeseries_smooth.tiff",
         device = "tiff",
         width = 12,
         height = 8,
       units = "in")

p_amp <-
  ggplot() +
  geom_smooth(data = filter(df_sum, type == "amp"),
              aes(x = date,
                  y = value,
                  color = type),
              method = "loess",
              span = 0.008) +
  geom_smooth(data = df_q,
              aes(x = date,
                  y = log(discharge.daily)),
              method = "loess",
              span = 0.008) +
  facet_wrap(~period, scales = "free_x") +
  # scale_color_manual(name = "", values = "blue") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  xlab("") +
  ylab("DO (mg/L)")
p_amp
ggsave(plot = p_min_max,
       filename = "Figures/dampierre_amp_timeseries_smooth.tiff",
       device = "tiff",
       width = 12,
       height = 8)

d_p1 <- df %>%
  filter(period == 1,
         site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(min = min(filtered),
            max = max(filtered)) %>%
  ungroup()

d_p2 <- df %>%
  filter(period == 2,
         site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(min = min(filtered),
            max = max(filtered)) %>%
  ungroup()

ggplot() +
  geom_line(data = d_p2,
             aes(x = date,
                 y = min))+
  geom_line(data = d_p2,
             aes(x = date,
                 y = max))

plot(d_p1$min)

# Test for stationarity
adf.test(d_p1$max)
kpss.test(d_p1$min)

diff_p1_min <- diff(d_p1$min,1)
diff_p1_max <- diff(d_p1$max,1)

diff_p2_min <- diff(d_p2$min,1)
diff_p2_max <- diff(d_p2$max,1)

adf.test(diff_p1_min)
kpss.test(diff_p1_min)

ccf(d_p1$min, d_p1$max)
ccf(d_p2$min, d_p2$max)

ccf(diff_p1_max, diff_p1_min, ylim = c(-0.1,0.5))
ccf(diff_p2_max, diff_p2_min, ylim = c(-0.1,0.5))

f <- decompose(ts(d_p1))

del <- 1  # sampling interval
x.spec <- spectrum(d_p2$max,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

spec.pgram(diff_p1_max, kernel("modified.daniell", c(5,7)))
acf(diff_p1_min)
ccf(d_p1$min, d_p1$max)
ccf(d_p2$min, d_p2$max)

p1_ccf <- ccf(diff_p1_max, diff_p1_min, ylim = c(-0.1,0.5))
p2_ccf <- ccf(diff_p2_max, diff_p2_min, ylim = c(-0.1,0.5))
df_ccf <- data.frame(period = "Period 1", Lag = p1_ccf$lag, CCF = p1_ccf$acf) %>%
  bind_rows(data.frame(period = "Period 2", Lag = p2_ccf$lag, CCF = p2_ccf$acf))

(ggplot(df_ccf) +
  geom_col(aes(x = Lag, y = CCF), width = 0.2) +
  facet_wrap(~period) +
  scale_x_continuous(limits = c(-7,7),
                     breaks = seq(-7, 7, 1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.5, 0.1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0.05,
            color = "blue",
            linetype = "dashed") +
  geom_hline(yintercept = -0.05,
            color = "blue",
            linetype = "dashed") +
    ggtitle("Daily max-min cross correlation")) %>%
  ggsave(filename = "Figures/max_min_ccf.tiff",
         device = "tiff",
         width = 12,
         height = 8,
         units = "in")




f <- decompose(ts(d_p1))

del <- 1/365  # sampling interval
x.spec <- spectrum(d_p1$min,log="no",span=20,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l", xlim = c(0,10))

spec.pgram(d_p2$max, kernel("modified.daniell", c(5,7)))
