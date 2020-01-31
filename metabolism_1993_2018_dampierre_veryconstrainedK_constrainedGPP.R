# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: September 10, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("D:/jake.diamond/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)
library(scales)
library(patchwork)

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

# Discharge data load and clean -----------------------------------------------------
# Generate daily time series
dat_seq <- data.frame(date = seq(ymd("1993-01-01"), 
                      ymd('2018-12-31'), 
                      by = "days"))

# Load discharge data, but this is missing 1994-1995, add 1995 data
df_q <- read_tsv("Data/Discharge/K4180010.txt") %>%
  mutate(date = ymd(paste(Annee, Mois, Jour, sep = "-"))) %>%
  rename(discharge.daily = Qm3s) %>%
  select(discharge.daily, date) %>%
  bind_rows(read_xlsx("Data/Discharge/gien_1995.xlsx") %>%
              mutate(date = ymd(date)) %>%
              rename(discharge.daily = discharge))

# Load 1994 data, but need to make average daily to match
df_q <- read_xls("Data/Moatar_thesis/DAM94AMC.XLS",
                     sheet = 4) %>%
  select(datetime = DATE, discharge = DEB) %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(discharge.daily = mean(discharge, na.rm = TRUE)) %>%
  bind_rows(df_q) %>%
  arrange(date) %>%
  right_join(dat_seq) %>%
  filter(between(date, ymd("1993-01-01"), ymd("2018-12-31"))) %>%
  distinct()

# Get rid of negative values
df_q$discharge.daily <- ifelse(df_q$discharge.daily < 0,
                               NA,
                               df_q$discharge.daily)

# Load light data and clean -------------------------------------
# Light data is in J/cm2/hr, need to convert to umol/m2/s
df_light <- read_excel("Data/Meteo/radiation_dampierre.xlsx") %>%
  select(site = NOM, datetime = DATE, light = GLO) %>%
  mutate(light = light * 10000*2.1/3600,
         datetime = ymd_h(datetime)) %>%
  filter(!(site == "SANCERRE" & datetime > ymd_h("2010-08-25-00"))) %>%
  select(-site)

# DO data load and clean --------------------------------------------------
# Load DO data and join
df <- readRDS("Data/all_DO_cleaned") %>%
  filter(site == "dampierre") %>%
  arrange(datetime) %>%
  left_join(readRDS("Data/all_DO_data") %>%
              select(var, site, datetime, value) %>%
              filter(var == "T", site == "dampierre") %>%
              mutate(var = recode(var,
                                  `T` = "temp.water")) %>%
              spread(var, value)) %>%
  left_join(readRDS("Data/Loire_DO/dampierre_temp_estimates"), by = "datetime") %>%
  mutate(temp.water = ifelse(is.na(temp.water.x), temp.water.y, temp.water.x)) %>%
  select(-date, -temp.water.y, -temp.water.x)

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Prepare data for stream Metabolizer -------------------------------------
# Florentina's DO.sat calculation
df$DO.sat <- ifelse(df$temp.water == 0,
                    0,
                    14.652 - 0.41022 * df$temp.water + 0.007991 * 
                      df$temp.water^2 - 0.000077774 * df$temp.water^3)

# Combine with light
df <- left_join(df, df_light)

# Convert to solar time at Gien station
df$solar.time <- calc_solar_time(df$datetime, longitude = 2.5)

# Caclculate light for periods without data (and to compare)
df$light_est <- calc_light(solar.time = df$solar.time,
                           latitude = 47.7,
                           longitude = 2.5)
df <- df %>%
  mutate(light = ifelse(is.na(light), light_est, light)) %>%
  select(-light_est)

# Calculate depth
depth <- df_q %>%
  mutate(depth = 0.134 * discharge.daily^0.4125)

# Get rid of datetime
df$datetime <- NULL

# Combine depth with streamMetabolizer data
df <- depth %>%
  right_join(df %>%
               mutate(date = date(solar.time))) %>%
  select(-date, -discharge.daily)

# Split data into five analysis periods to reduce memory needed for each run
df2 <- df %>%
  mutate(year = year(solar.time),
         time_frame = as.numeric(cut(year, 5))) %>%
  select(DO.obs = DO_use, temp.water, site, 
         light, depth, DO.sat, solar.time
         , time_frame
  ) %>%
  distinct()

# Create periods of 4–6 years and nest data
df_n <- df2 %>%
  filter(site == "dampierre") %>%
  select(-site) %>%
  group_by(time_frame) %>%
  nest() %>%
  left_join(df_q %>%
              mutate(time_frame = as.numeric(cut(year(date), 5))) %>%
              group_by(time_frame) %>%
              nest() %>%
              rename(data_q = data))
# Plot the data -----------------------------------------------------------
p_q <- ggplot(data = df_q %>%
                mutate(date = ymd(date)) %>%
                group_by(year(date), month(date)) %>%
                summarize(med_q = median(discharge.daily, na.rm = TRUE)) %>%
                mutate(date = ymd(paste(`year(date)`,
                                        `month(date)`,
                                        "01",
                                        sep = "-"))),
              aes(x = date,
                  y = med_q)) + 
  theme_bw() + geom_line() +
  scale_x_date(limits = c(ymd("1994-10-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                            "5 years"),
               labels = date_format("%Y"),
               minor_breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                                  "1 years")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab("") +
  ylab(expression(atop("Discharge","("*m^3~s^{-1}*")")))
p_q

p_light <- ggplot(data = df_light %>%
                    group_by(year(datetime), month(datetime)) %>%
                    filter(light > 0) %>%
                    summarize(med_l = median(light, na.rm = TRUE)) %>%
                    mutate(date = ymd(paste(`year(datetime)`,
                                            `month(datetime)`,
                                            "01",
                                            sep = "-"))),
                  aes(x = date,
                      y = med_l)) + 
  theme_bw() + geom_line() +
  scale_x_date(limits = c(ymd("1994-10-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                            "5 years",
               ),
               minor_breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                                  "1 years"),
               labels = date_format("%Y")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab("") +
  ylab(expression(atop("PAR","("*mu*"mol"~m^{-2}~s^{-1}*")")))
p_light

p_temp <- ggplot(data = df %>%
                   group_by(year, month) %>%
                   summarize(med_t = median(temp.water, na.rm = TRUE)) %>%
                   mutate(date = ymd(paste(year,
                                           month,
                                           "01",
                                           sep = "-"))),
                 aes(x = date,
                     y = med_t)) + 
  theme_bw() + geom_line() +
  scale_x_date(limits = c(ymd("1994-10-01"), ymd("2020-01-01")),
               breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                            "5 years"),
               minor_breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                                  "1 years"),
               labels = date_format("%Y")) +
  theme(axis.title.x = element_blank()) +
  xlab("") +
  ylab(expression(atop("Water temperature", "("*degree*C*")")))
p_temp

# Plot all that data in monthly form
(p_q/ p_light/ p_temp)%>%
  ggsave(filename = "Figures/Middle_Loire/monthly_inputs_summary.png",
         device = "png",
         dpi = 300,
         width = 6,
         height = 6,
         units = "in")
# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_mod <- mm_name(type = 'bayes', 
                     pool_K600 = 'binned', 
                     err_obs_iid = TRUE, 
                     err_proc_iid = TRUE)
bayes_mod

# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, data_q, bayes_name = bayes_mod){
  # Calculate the natural-log-space centers of the discharge bins
  # These are the bins for the time frame of 
  # Use the width method as in the help file with with = 0.8 log units
  brks <- calc_bins(vec = log(data_q$discharge.daily),
                    method = "width",
                    width = 0.8)$bounds
  
  # Estimate the mean ln(k600) value for the river from O'Connor and direct 
  # measurements with floating dome
  # Theis are the hyperprior mean for k600 in log space 
  k6 <- 1
  
  # Same for standard deviation, super tight prior
  k6_sd <- 0.3
  
  # Set the specifications
  bayes_specs <- specs(model_name = bayes_name,
                       burnin_steps = 1000,
                       saved_steps = 500
                       , K600_lnQ_nodes_centers = brks
                       , K600_lnQ_nodes_meanlog = rep(k6, 
                                                      length(brks))
                       , K600_lnQ_nodes_sdlog = rep(k6_sd, 
                                                    length(brks))
                       , GPP_daily_mu = 8 
                       , GPP_daily_sigma = 6
  )
  
  # Do the metabolism
  metab(specs = bayes_specs, 
        data = as.data.frame(data), 
        data_daily = as.data.frame(data_q))
}

# Run the metabolism model on nested data ---------------------------------
mm_all <- df_n %>%
  transmute(mm = map2(data, data_q, ~met_fun(data = .x,
                                             data_q = .y)
  )
  )

saveRDS(mm_all, "F:/DataLocalePerso/Jake/metab_veryconstrainedK_gppconstrained")
# Inspect the model -------------------------------------------------------
mm <- mm_all %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met)

gpp <- ggplot(data = mm, aes(x = date,
                      y = GPP)) + geom_point()
gpp
# mm_all <- readRDS("Data/Loire_DO/metab_veryconstrainedK_err_proc_gpp_true")
mm %>%
  mutate(year = year(date),
         month = month(date)) %>%
  select(year, date, GPP, ER, month) %>%
  filter(GPP > 0, ER < 0) %>%
  gather(flux, value, -year, -date, -month) %>%
  group_by(year, flux) %>%
  summarize(avg = mean(value, na.rm = T)) %>%
  ggplot(aes(x = year, y = avg, color = flux)) + geom_point()

rh <- mm_all[4,] %>%
  mutate(r = map(mm, get_fit)) %>%
  unnest(r)
  select(ends_with("Rhat"))
get_fit(mm_all)


kt <- mm_all %>%
  mutate(mk = map(mm, get_params)) %>%
  unnest(mk)
plot(kt$K600.daily, kt$ER.daily)
plot(kt$date, kt$K600.daily)
kt %>%
  filter(ER.daily < 0, GPP.daily > 0) %>%
  ggplot(aes(date, K600.daily)) + geom_point()

# Look at priors/posteriors
plot_distribs(bayes_specs, "K600_daily_lnQ")
plot_distribs(mm_all[1,], "K600_daily")
get_specs(mm_all)
# Daily metabolism predictions
predict_metab(mm)
plot_metab_preds(mm_all)
get_specs(mm)
get_params(mm)
plot_DO_preds(mm)
plot(get_params(mm_all)$K600.daily, predict_metab(mm_all)$ER)
plot(get_params(mm)$date, get_params(mm)$K600.daily)
head(predict_DO(mm))
get
?plot_DO_preds()
saveRDS(mm, file = "Data/Loire_DO/mm_1993_1996")
mm <- readRDS("Data/Loire_DO/mm_1994.rds")

do_test <- lag(get_data(mm)$DO.obs) + 
  (predict_DO(mm)$DO.mod-lag(predict_DO(mm)$DO.mod))+
  summary(get_fit(mm)$inst$err_proc_iid_mean)
summary(get_fit(mm)$inst$err_obs_iid_mean)
plot(do_test)
