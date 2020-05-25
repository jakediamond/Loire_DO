# 
# Purpose: To do nitrogen analysis for middle Loire River
# Author: Jake Diamond
# Date: April 1, 2020
# 

# Set working directory
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(ggsci)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(plotly)
library(mcp)
library(changepoint)
library(imputeTS)
library(tidyverse)

# Set seed
set.seed(42)

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm"))))

# Read in water chemistry data and pair it with GPP/depth for the same day
df <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  left_join(readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")) %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  mutate(depth = 0.134 * discharge.daily^0.4125)

# Clean the data by removing NAs
df <- df %>%
  filter_at(vars(value), all_vars(!is.na(.)))

# Take a look at the data
p <- ggplot(data = dplyr::filter(df, solute %in% c( "NO3"),
                                 between(month, 5, 10)) %>%
              group_by(year) %>%
              summarize(min = min(value, na.rm = TRUE)),
            aes(x = year,
                y = min)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 8)) +
  theme_bw()
p
ggplotly(p)

# Note that as soon as chlorophyll a drops off around 2004/2005, summertime
# minima of nitrate increase...calculate this difference
# First calculate autotrophic uptake in g N/m3/d (= mg/L/d)
df_uptake <- df %>%
  dplyr::filter(solute %in% c("CHLA", "NO3"),
                between(month, 5, 10)) %>%
  group_by(year, solute) %>%
  mutate(uptake_alg = GPP * (12/32) / 8,
         uptake_mac = GPP * (12/32) / 20) %>%
  select(date, year, month, solute, value, uptake_alg, uptake_mac) %>%
  mutate(period = if_else(year < 2006, 0, 1)) %>%
  dplyr::filter(uptake_alg > 0) %>%
  mutate(uptake = if_else(period == 0, uptake_alg, uptake_mac))

ggplot(data = dplyr::filter(df_uptake, solute == "CHLA"),
       aes(x = date,
           y = uptake,
           color = period,
           group = period)) +
  geom_point()

# Take a look at uptake~chla for period before change
ggplot(data = dplyr::filter(df_uptake, solute == "CHLA"),
       aes(x = value,
           y = uptake_alg,
           color = period,
           group = period)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y~x)

# Clean data for regression
reg_data <- dplyr::filter(df_uptake, period == 1,
                          solute == "CHLA")

# Get the lm (IRLS) results for the first period
mod_pd1 <- glm(uptake_alg ~ value, 
                   data = reg_data)
summary(mod_pd1)

# Apply the lm to the second period and extract residuals
pred <- predict(mod_pd1, newdata = dplyr::filter(df_uptake, period == 2,
                                                  solute == "CHLA"))
resid <- dplyr::filter(df_uptake, period == 2,
                       solute == "NO3")$uptake_alg - pred
plot(resid)

dplyr::filter(df_uptake, period == 2, solute == "NO3") %>%
  ungroup() %>%
  mutate(resid = resid) %>%
  dplyr::filter(between(month, 5, 10)) %>%
  left_join(select(df_updown, date, diff_n)) %>%
  ggplot(aes(x = diff_n,
             y = resid,
             color = month)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  # scale_x_date(date_breaks = "2 years") +
  scale_color_viridis_c()
  

# We have higher uptake than expected in the macrophyte period than would be
# expected based on chlorophyll-a; this is due to macrophyte uptake.
# Calculate expected macrophyte uptake for second period and compare


# Calculate maxima and minima by year
df_amp <- df %>%
  filter(solute == "NO3") %>%
  group_by(year) %>%
  summarize(max = max(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            amp = max - min)

# Calculate cumulative uptake
df_cum_uptake <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  mutate(depth = 0.134 * discharge.daily^0.4125,
         year = year(date),
         month = month(date)) %>%
  select(year, month, date, GPP, depth, discharge.daily) %>%
  distinct(date, .keep_all = TRUE) %>%
  filter(year > 1992) %>%
  group_by(year) %>%
  mutate(GPP = if_else(GPP<0 | is.na(GPP), 0, GPP),
         uptake_alg = GPP * (12/32) / 8 / depth,
         uptake_mac = GPP * (12/32) / 20 / depth) %>%
  mutate(cum_up = cumsum(uptake_alg))




df_wq <- readRDS("Data/Loire_DO/all_longterm_wq_data") %>%
  bind_rows(read_xlsx("Data/Loire_DO/tabPC.xlsx") %>%
              dplyr::rename(site_no = cd_site,
                            date = date_opecont,
                            TEMP_EAU = `Temp. eau`,
                            COND = `Conductiv.25°C`,
                            ChOD = DCO,
                            NKj = NKJ,
                            O2 = `O2 dissous`,
                            NH4 = `NH4+`,
                            NO3 = `NO3-`,
                            SIO = `SiO2`,
                            PTO = `P total`,
                            PO4 = `Orthophosp`,
                            PheoP = `PHEOPIG.`,
                            CHLA = `CHL.A`) %>%
              dplyr::select(-`Conductiv.20°C`, -cd_opecont) %>%
              mutate(date = as.Date(date),  
                     site_no = str_pad(site_no, 8, pad = "0"),
                     Annee = year(date),
                     Mois = month(date),
                     Jour = day(date)))

# Bit of cleaning
df_wq <- df_wq %>%
  distinct() %>%
  rename(DOC = COD,
         TKN = NKj,
         SiO2 = SIO,
         temp = TEMP_EAU,
         SC = COND,
         TP = PTO,
         DO = O2,
         SPM = MES,
         BOD5 = DBO5,
         year = Annee,
         month = Mois,
         day = Jour)

# Just middle Loire sites
df_wq <- df_wq %>%
  mutate_all(. , list(~na_if(., -1))) %>%
  filter(site_no %in% c(#"04045900",
                                      "04046800",
                                      # "04046000",
                                      # "04046400",
                                      "04048000",
                                      "04049000",
                                      "04050500"
                                      )) %>%
  distinct(site_no, date, .keep_all = TRUE) %>%
  mutate(loc = if_else(site_no %in% c(#"04045900",
                                      "04046800"),
                                      # "04046000",
                                      #"04046400"), 
                       "upstream",
                       "downstream"))
df_wq <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  mutate(NO3 = NO3 * 14.01 / 62,
         NH4 = NH4 * 14.01 / 18,
         TN = NO3 + TKN) %>%
  pivot_longer(cols = -c(1:5), names_to = "solute", values_to = "value")

# Da for N
df_da <- df_wq %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  ungroup() %>%
  dplyr::filter(solute == "NO3") %>%
  left_join(distinct(dplyr::select(ungroup(df_uptake), date, uptake), .keep_all = TRUE)) %>%
  mutate(rxn = abs(uptake), #reaction in units of gN/d/m2
         trans = discharge.daily * ((value)*86400) / 200, #transport in gC/d/m2 assuming 1 m depth and 200 m wide
         Da = rxn / trans,
         year = year(date)) %>%  
  group_by(year) %>%
  summarize(Da = mean(Da, na.rm = TRUE))

# Plot
p_da <- ggplot(data = df_da,
               aes(x = year,
                   y = Da,
                   # color = as.factor(regime),
                   group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_continuous(limits = c(1993, 2018),
                     breaks = seq(1993, 2018, 2)) +
  # scale_color_aaas(name = "regime",
  #                  breaks = c(0,1),
  #                  labels = c("phytoplankton", "macrophytes")) +
  # guides(color = FALSE) +
  annotation_logticks(
    base = 10,
    sides = "l") +
  # annotate(geom = "rect", xmin = 2011,
  #          xmax = 2013,
  #          ymin = min(df_dasum$Da),
  #          ymax = max(df_dasum$Da),
  #          alpha = 0.4,
  #          fill = "dark blue") +
  ylab(expression("Mean annual Damkohler (ER:river C flux)")) +
  theme(axis.title.x = element_blank()) +
  labs(subtitle = "C")
p_da


df_wq_sum <- df_wq %>%
  group_by(year, solute) %>%
  summarize(max = max(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            mean = mean(.[between(.$month, 6, 9), "value"]))

(ggplot(data = filter(df_wq, solute %in% c("NO3", "TKN"),
                     between(month, 6, 9)) %>%
         group_by(year, solute) %>%
         summarize(value = mean( value, na.rm = T)),
       aes(x = year,
           y = value,
           fill = solute)) +
  geom_col() +
  # geom_smooth(se = FALSE) +
  scale_fill_aaas() +
  # geom_vline(xintercept = ymd("2006-01-01"), linetype = "dashed") +
  # scale_x_date(date_breaks = "5 years",
  #              limits = c(dmy("01-01-1980"), dmy("01-01-2020"))) +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 5)) +
  ylab("summer nitrogen (mg N/L)") +
  xlab("") +
  theme(legend.position = c(0.2, 0.8))) %>%
  ggsave(filename = "Figures/Middle_Loire/summer_nitrogen_breakdown_bar.png",
         width = 10,
         height = 10,
         units = "cm",
         device = "png",
         dpi = 300)

ggplot(data = filter(df_wq_sum, solute %in% c("NO3", "TKN", "TN")),
       aes(x = year,
           y = max,
           color = solute)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  scale_color_aaas() +
  # geom_vline(xintercept = ymd("2006-01-01"), linetype = "dashed") +
  # scale_x_date(date_breaks = "5 years",
  #              limits = c(dmy("01-01-1980"), dmy("01-01-2020"))) +
  scale_y_continuous(limits = c(0, 5))

# Add discharge for st satur
read_delim("Data/Discharge/Export2019/K4000010_qj_hydro2_v2.txt", delim = ";") %>%
  mutate(date = ymd(date),
         discharge = discharge / 1000) %>%
  select(date, q_sat = discharge) %>%
  right_join(df_wq) -> df_wq

df_updown <- df_wq %>%
  select(loc, date, year, month, NO3, CHLA, q_sat) %>%
  group_by(date, loc) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  pivot_wider(names_from = loc, values_from = c(NO3, CHLA)) %>%
  left_join(distinct(select(ungroup(df_cum_uptake), date, 
                            GPP, depth, discharge.daily), 
                     date, .keep_all= TRUE)) %>%
  mutate(diff_no3 = NO3_upstream - NO3_downstream,
         diff_chla = CHLA_upstream - CHLA_downstream,
         diff_n = (NO3_upstream * q_sat - NO3_downstream * discharge.daily),
         n_uptake = -GPP * (12/32) *200 * 10000 / 8,
         diff_uptake = n_uptake - diff_n) 

  # mutate(needed_CN = GPP * (12/32) / diff_no3 / depth)

ggplot(data = filter(df_updown, diff_chla < 0),
       aes(x = -diff_chla,
           y = -diff_no3)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2))

ggplot(data = filter(df_updown, between(month, 5, 10)),
                     #discharge.daily < 200),
       aes(x = date,
           y = -diff_n * 86400 / 1000 / 50,
           color = month)) +
  geom_point() +
  scale_y_continuous(limits = c(-500,500)) +
  scale_x_date(limits = c(ymd("1980-01-01", "2020-01-01"))) +
  scale_color_viridis_c() +
  geom_hline(yintercept = 0) +
  stat_smooth(method = "lm") +
  ylab(expression(Delta*NO[3]^{`-`}*"–N"~"(kg"~d^{-1}~km^{-1}*")")) +
  theme(axis.title.x = element_blank())
