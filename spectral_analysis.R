# 
# Purpose: To look at spectral signatures of DO in EDF time series
# Author: Jake Diamond
# Date: November 8, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidyverse)
library(tseries)

df <- readRDS("Data/all_DO_cleaned")

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
