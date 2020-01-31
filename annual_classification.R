# # 
# Purpose: To classify years by summertime hydrology, temperature, radiation
# Author: Jake Diamond
# Date: November 11, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(streamMetabolizer)
library(tidyverse)
library(tseries)
library(readxl)
library(hydrostats)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# Load temp data
df <- readRDS("Data/all_DO_data") %>%
  dplyr::select(var, site, datetime, value) %>%
  dplyr::filter(var == "T", site == "dampierre") %>%
  mutate(var = dplyr::recode(var,
                      `T` = "temp.water")) %>%
  spread(var, value) %>%
  bind_rows(readRDS("Data/Loire_DO/dampierre_temp_estimates")) %>%
  arrange(datetime) %>%
  dplyr::select(-date, -site)

# Load Q data
# # Generate daily time series
dat_seq <- data.frame(Date = seq(ymd("1993-01-01"), 
                                 ymd('2018-12-31'), 
                                 by = "days"))
# Load discharge data, but this is missing 1994-1995
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  dplyr::select(Q = Qm3s, Date = date)

# Load 1994 and 1995 data, but need to make average daily to match
df_q <- read_xls("Data/Moatar_thesis/DAM95AMC.XLS",
                 sheet = 1) %>%
  bind_rows(read_xls("Data/Moatar_thesis/DAM94AMC.XLS",
                     sheet = 4)) %>%
  dplyr::select(datetime = DATE, Q = DEB) %>%
  mutate(Date = date(datetime)) %>%
  group_by(Date) %>%
  dplyr::summarize(Q = mean(Q, na.rm = TRUE)) %>%
  # drop_na() %>%
  bind_rows(df_q) %>%
  arrange(Date) %>%
  right_join(dat_seq) %>%
  dplyr::filter(between(Date, ymd("1993-01-01"), ymd("2018-12-31"))
  ) %>%
  distinct() %>%
  mutate(Q = ifelse(Q < 0, NA, Q),
         Date = as.POSIXct(Date))

df_q_stat <- df_q %>%
  dplyr::filter(between(month(Date), 4, 10)) %>%
  group_by(year(Date)) %>%
  nest() %>%
  mutate(ls = map(data, high.spells, threshold = 150)) %>%
  unnest(ls)

# Make it less than 70 cms in summer, temperature number of days higher than 26
# Hydrostatistics
df_q_stat <- df_q %>%
  dplyr::filter(between(month(Date), 4, 10)) %>%
  group_by(year(Date)) %>%
  nest() %>%
  mutate(ls = map(data, low.spells, threshold = 70),
         hs = map(data, high.spells, threshold = 70)) %>%
  unnest(ls, hs) %>%
  mutate(med = map(data, ~median(dplyr::filter(., between(month(Date),
                                                   5,
                                                   10)
                                        )$Q, na.rm = TRUE))) %>%
  unnest(med) %>%
  select_if(~!all(is.na(.)))

# Temperature statistics
df_t_stat <- df %>%
  mutate(date = date(datetime),
         month = month(date),
         year = year(date)) %>%
  group_by(date, month, year) %>%
  dplyr::summarize(t = mean(temp.water, na.rm = TRUE)) %>%
  dplyr::filter(between(month, 4, 10)) %>%
  group_by(year) %>%
  dplyr::summarize(t_med = median(t, na.rm = TRUE),
            t_80 = quantile(t, 0.8, na.rm = TRUE))
  
# Light statistics
df_light <- read_excel("Data/Meteo/radiation_dampierre.xlsx") %>%
  dplyr::select(site = NOM, datetime = DATE, light = GLO) %>%
  mutate(light = light * 10000*2.1/3600,
         datetime = ymd_h(datetime)) %>%
  dplyr::filter(!(site == "SANCERRE" & datetime > ymd_h("2010-08-25-00"))) %>%
  dplyr::select(-site) %>%
  bind_rows(df %>%
              dplyr::filter(date(datetime) > ymd("2017-12-31")) %>%
              dplyr::select(datetime) %>%
              mutate(solar.time = calc_solar_time(datetime, longitude = 2.5),
                     light = calc_light(solar.time = solar.time,
                                        latitude = 47.7,
                                        longitude = 2.5)*0.8) %>%
              dplyr::select(datetime = solar.time, light)
            ) %>%
  mutate(date = date(datetime),
         month = month(date),
         year = year(date)) %>%
  group_by(date, month, year) %>%
  dplyr::summarize(light = max(light, na.rm = TRUE)) %>%
  dplyr::filter(between(month, 4, 10)) %>%
  group_by(year) %>%
  summarize(l_med = median(light, na.rm = TRUE),
            l_80 = quantile(light, 0.8, na.rm = TRUE))

# combine all data for classification
df_class <- df_q_stat %>%
  dplyr::select(year = `year(Date)`, -data,
         med) %>%
  # left_join(df_light) %>%
  left_join(df_t_stat) %>%
  column_to_rownames(var = "year") %>%
  drop_na()

# scale data
df_class_scale <- scale(df_class)

# Dissimilarity matrix
d <- dist(df_class_scale, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(df_class_scale, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df_class_scale, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df_class_scale, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 3)

# Number of members in each cluster
table(sub_grp)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 2, border = 2:5)

fviz_nbclust(df_class_scale, FUN = hcut, method = "silhouette")
