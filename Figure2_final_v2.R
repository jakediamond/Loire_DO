# 
# Purpose: To plot Figure 2 for the paper
# Author: Jake Diamond
# Date: April 8, 2020
# 

# Set working directory
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lmtest)
library(furrr)
library(broom)
library(tseries)
library(scales)
library(ggsci)
library(imputeTS)
library(patchwork)
library(lubridate)
library(tidyverse)


# Set the plotting theme
theme_set(theme_bw(base_size=8)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
                  plot.tag.position = c(0.15,0.95)))

# Load metabolism data (and interpolated chlorophyll a data)
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  left_join(read_excel("Data/Chla_GPP_11 stations_Gien to Villandry_et HFCM.xlsx", sheet = 4) %>%
              dplyr::select(date = dateinterp, chla = chlainterp) %>%
              mutate(date = as.Date(date)))

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Load middle loire water quality data
df_wq <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  ungroup() %>%
  group_by(solute, year, month) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  filter(year > 1979) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  dplyr::select(solute, date, value)

# Load macrophtye data
df_mac <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>% #outlier species
  summarize(area = sum(surface_area, na.rm = TRUE))

# First panel, Granger causality ------------------------------------------
# Nest data
df_met_n <- df_met %>%
  ungroup() %>%
  # dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER,
         chla = ifelse(chla < 0, NA, chla),
         year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  na.trim(.) %>%
  as.data.frame(.) %>%
  na_kalman(., maxgap = 3) %>% #fill gaps of up to 3 days with kalman filter
  as_tibble(.) %>%
  group_by(year) %>%
  nest()

# test if data is stationarity, do granger causality
df_gc <- df_met_n %>%
  #mutate(stat = future_map(data, ~adf.test(diff(.$chla))$p.val)) # commented out for now, but tests for stationarity
  mutate(summer_data = map(data, ~ dplyr::filter(., between(.$month, 5, 9))), #period of actual signal
         gc_er_gpp = map(summer_data, ~ grangertest(diff(.$ER) ~ diff(.$GPP),
                                                   order = 1)),
         gc_gpp_chla = map(data, ~grangertest(.$GPP ~ .$chla,
                                                     order = 1))) %>%
  unnest(c(gc_er_gpp, gc_gpp_chla), names_sep = ".") %>%
  mutate(regime = if_else(year < 2013, 0, 1))

# plotting dataframe
df_gc_p <- df_gc %>%
  dplyr::select(year, regime, contains("Pr")) %>%
  ungroup() %>%
  drop_na() %>%
  pivot_longer(cols = -c(year, regime), 
               names_to = "causality", 
               values_to = "pval") %>%
  mutate(causality = case_when(grepl("chla", causality) ~ "GPP ~ chlorophyll a",
                               grepl("er", causality) ~ "ER ~ GPP"))

p_grang <- ggplot(data = df_gc_p,
                  aes(x = year,
                      y = pval,
                      color = as.factor(regime),
                      shape = causality,
                      group = causality)) +
  geom_point() +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_aaas(name = "metabolic regime",
                    breaks = c(0,1),
                    labels = c("phytoplankton", "macrophytes")) +
  # guides(color = TRUE) +
  annotate(geom = "rect", xmin = 2013,
            xmax = 2014,
            ymin = -Inf,
            ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
  annotate("segment", x = 2008, xend = 2013, y = 0.15, yend = 0.15, 
           colour = "black", arrow=arrow(length = unit(0.2,"cm"))) +
  annotate("segment", x = 1996, xend = 1993, y = 0.15, yend = 0.15, 
           colour = "black", arrow=arrow(length = unit(0.2,"cm"))) +
  annotate(geom = "text",
           x = 2002,
           y = 0.15,
           label = "GPP and ER tightly coupled",
           size = 2.5) +
  geom_line() +
  theme(legend.position = c(0.38, 0.65),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.key.height = unit(0.2, "cm"),
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = -0.1, unit='cm')) + 
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1993, 2018, 2),
                     limits = c(1993, 2018)) +
  xlab("") +
  ylab(expression(Granger~causality~p-val))
p_grang

# Second panel, NEP over time ---------------------------------------------
p_nep <- df_met %>%
  filter(between(month, 5, 9)) %>%
  group_by(year) %>%
  summarize(NEP_mean = mean(NEP, na.rm = TRUE),
            NEP_sd = sd(GPP, na.rm = TRUE),
            NEP_n= n()) %>%
  mutate(NEP_se = NEP_sd / sqrt(NEP_n),
         lower.ci.NEP = NEP_mean - qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se,
         upper.ci.NEP = NEP_mean + qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se,
         regime = if_else(year < 2013, 0, 1)) %>%
  ggplot(aes(x = year,
             y = NEP_mean,
             color = as.factor(regime),
             group = 1)) +
  geom_line() +
  geom_point() +
  annotate(geom = "rect", xmin = 2013,
           xmax = 2014,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
  geom_errorbar(aes(ymin = lower.ci.NEP, ymax = upper.ci.NEP), width =0.2)  +
  geom_hline(yintercept = 0) +
  scale_color_aaas(name = "metabolic regime",
                   breaks = c(0,1),
                   labels = c("phytoplankton", "macrophytes")) +
  scale_x_continuous(breaks = seq(1993, 2018, 2),
                     limits = c(1993, 2018)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * -250*44*180*12/32/44/1e3, 
                                         name = expression("summer C efflux (kg C "*km^{-1}*")"))) +
  theme(legend.position = c(0.2, 0.18),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.key.height = unit(0.2, "cm"),
        legend.margin = margin(t = 0, b = -0.1, unit='cm')) + 
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))
p_nep
# Third panel, Damkohler number -------------------------------------------
# First, estimate annual scaled ratio of macrophytes to chla
# to determine relative CN uptake 
df_ratio <- df_wq %>%
  filter(solute == "CHLA",
         between(month(date), 7, 9),#want period of highest chlorophyll
         year(date) > 1992) %>% 
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(chla = mean(value, na.rm = TRUE)) %>%
  left_join(df_mac %>%
              bind_rows(tibble(year = 2015, area = NA)) %>%
              arrange(year) %>%
              mutate(area = na_interpolation(area))) %>% # linear interpolate for 2015, missing data
  mutate(ratio = if_else(is.na(area / chla), 0, area / chla)) %>%
  mutate_at(vars(ratio), rescale, to = c(0.001, 1)) %>%
  dplyr::select(year, ratio)

# join data together to calculate damkohler; use monthly averages
df_da <- left_join(df_met, df_q) %>%
  mutate(depth = 0.134 * discharge.daily^0.4125) %>% #get depth from discharge data, site specific eqn
  group_by(year, month) %>%
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE),
            q = mean(discharge.daily, na.rm = TRUE),
            depth = mean(depth, na.rm = TRUE),
            chla = mean(chla, na.rm = TRUE)) %>%
  na_kalman() %>%
  right_join(pivot_wider(df_wq, names_from = solute, values_from = value) %>%
              dplyr::select(date, DOC, CHLA, PheoP, NO3, TKN, NH4) %>%
              mutate(month = month(date),
                     year = year(date)) %>%
              group_by(month) %>% 
              mutate(DOC = if_else(is.na(DOC), #replace NA DOC with mean monthly/sd DOC
                                   mean(DOC, na.rm = TRUE) - 2 + 
                                     sd(DOC, na.rm = TRUE), DOC) / 1.5) %>% 
              ungroup()) %>%
  mutate(uptake_alg = GPP * (12/32) / 8, # calculate autotrophic nitrogen uptake based on stoichiometry
         uptake_mac = GPP * (12/32) / 20) %>%  #C/N = 8:1 for algae, 20:1 for macro
  right_join(df_ratio) %>%
  mutate(uptake = if_else(year < 2005, uptake_alg, uptake_mac)) %>%
           # uptake_alg * (1 - ratio) + uptake_mac * ratio) %>%
  mutate(POC = 32 * (CHLA + PheoP)/1000) %>% #conversion of pigments to POC from Minaudo et al. 2016
  na_kalman() %>% #interpolate remaining NAs
  mutate(TOC = POC + DOC, #total organic carbon
         NO3 = NO3 * 14.01 / 62, #get as N
         NH4 = NH4 * 14.01 / 18, #get as N
         TIN = NO3 + NH4)

# Calculate dahmkohler number by monthly averages
df_dasum <- df_da %>%
  mutate(rxnC = abs(ER) * 12/32 * 10000 * 250, #reaction in units of gC/d over 10 km reach
         transC = ((q * TOC) * 86400), #transport in gC/d/m2 assuming 250 m wide
         DaC = rxnC / transC) %>%
  mutate(rxnN = abs(uptake) * 10000 * 250, #reaction in units of gN/d/m2
         transN = ((q * TIN) * 86400), #transport in gN/d/m2 assuming 250 m wide
         DaN = rxnN / transN) %>%
  group_by(year) %>%
  # mutate(nlog = log(DaN),
  #        clog = log(DaC)) %>%
  # summarize(meanN = mean(nlog, na.rm = TRUE),
  #           meanC = mean(clog, na.rm = TRUE),
  #           sdC = sd(clog, na.rm = TRUE),
  #           sdN = sd(clog, na.rm = TRUE),
  #           DaC = exp(meanC + 0.5*sdC^2),
  #           DaN = exp(meanN + 0.5*sdN^2)) %>%

            # sdl = ml*sqrt(exp(sd^2)-1),
            # sel = sdl / sqrt(n()))
  summarize(DaC = sum(rxnC) / sum(transC),
            DaN = sum(rxnN) / sum(transN)) %>%
  # summarize(DaC = mean(DaC, na.rm = TRUE),
  #           DaN = mean(DaN, na.rm = TRUE)) %>%
  dplyr::select(year, DaC, DaN) %>%
  mutate(regime = if_else(year < 2013, 0, 1)) %>%
  pivot_longer(cols = -c(year, regime), 
               names_to = "element", 
               names_prefix = "Da",
               values_to = "Da")

p_da <- ggplot(data = df_dasum,
       aes(x = year,
           y = Da,
           color = as.factor(regime),
           group = element)) +
  geom_point(aes(shape = element)) +
  geom_line() +
  scale_shape_manual(name = "element",
                       breaks = c("C", "N"),
                       values = c(16, 1)) +
  scale_linetype_manual(name = "element",
                          breaks = c("C", "N"),
                          values = c("solid", "long dash")) +
  scale_y_log10(breaks = c(10^-3, 10^-2, 10^-1),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(10^-3, 10^-0.5),
                expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1993, 2018, 2)) +
  scale_color_aaas(name = "metabolic regime",
                   breaks = c(0,1),
                   labels = c("phytoplankton", "macrophytes")) +
  annotation_logticks(
    sides = "l") +
  annotate(geom = "rect", xmin = 2013,
           xmax = 2014,
           ymin = 10^-3,
           ymax = 10^-0.5,
           alpha = 0.4,
           fill = "dark blue") +
  annotate(geom = "text",
           x = 2006,
           y = 10^-0.8,
           label = "ER:TOC river flux",
           size = 2.5) +
  annotate(geom = "text",
           x = 2000.5,
           y = 10^-2.8,
           label = "autotrophic nitrate uptake:TIN river flux",
           size = 2.5) +
  ylab(expression("mean annual Damkohler")) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.7, 0.55),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.height = unit(0.2, "cm"),
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = -0.1, unit='cm'))
p_da

# Fourth panel, change in controls on GPP ---------------------------------
# Read in regression data
df_reg <- read_csv2("data_for_regression_v2.csv") %>%
  mutate(period = if_else(period == "pre", 0, 1))

df_use_subs <- df_reg %>%
  # mutate(tf = if_else(year(date) > 2005, "post", "pre")) %>%
  drop_na() %>%
  dplyr::select(-date, -max_light, -ER) %>%
  as.data.frame()

# Scaled data
df_reg_scale <- df_reg %>%
  transmute_at(4:12, scale) %>%
  add_column(GPP = df_reg$GPP,
             period = df_reg$period,
             date = df_reg$date) %>%
  filter(between(month(date), 5, 9)) %>%
  drop_na() %>%
  # dplyr::select(-date) %>%
  as.data.frame()

# Interactions
fit_int <- lm(GPP ~ (discharge +med_light) *period, data = df_reg_scale)
summary(fit_int)
# anova(fit_ints, fit_int2)
# interact_plot(fit_int, pred = med_light, modx = period)

# Make tidy
df_cont <- tidy(fit_int) %>%
  filter(!(term %in% c("(Intercept)", "period"))) %>%
  mutate(period = if_else(str_detect(term, ":"), 1, 0),
         factor = word(term, 1, sep = ":")) %>%
  group_by(factor) %>%
  mutate(slope = if_else(period == 0,
                         estimate,
                         sum(estimate))) %>%
  ungroup() %>%
  mutate(factor = recode(factor, discharge = "daily mean discharge", 
                         med_light = "median daily PAR"))
# Plot the data
p_cont <- ggplot(data = df_cont,
                 aes(x = factor,
                     y = slope,
                     fill = as.factor(period))) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = slope - std.error,
                    ymax = slope + std.error),
                position=position_dodge(width = 0.9), 
                colour="black", width = 0.3) +
  geom_hline(yintercept = 0) +
  scale_fill_aaas(name = "metabolic regime",
                  breaks = c(0,1),
                  labels = c("phytoplankton", "macrophytes")) +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("environmental driver") +
  ylab("linear response slope of GPP")
  
p_cont
# Final figure ------------------------------------------------------------

# Plot all plots
(((p_grang/ p_da) | (p_nep / p_cont)) +
   plot_annotation(tag_levels = 'a')) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure2_final.svg",
         device = "svg",
         dpi = 300,
         height = 100,
         width = 183,
         units = "mm")

