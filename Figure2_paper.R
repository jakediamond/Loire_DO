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
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm"))))

# Load metabolism data (and interpolated chlorophyll a data)
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  left_join(read_excel("Data/Chla_GPP_11 stations_Gien to Villandry_et HFCM.xlsx", sheet = 4) %>%
              select(date = dateinterp, chla = chlainterp) %>%
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
  select(solute, date, value)

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
  # na_seadec(., find_frequency = TRUE, algorithm = "kalman") %>%
  # mutate(gpp = if_else(is.na(GPP), 0, GPP),
  #        er = if_else(is.na(ER), 0, ER)) %>%
  as_tibble(.) %>%
  group_by(year) %>%
  nest()

# test if data is stationarity, do granger causality
df_gc <- df_met_n %>%
  # mutate(stat = future_map(data, ~adf.test(diff(.$gpp))$p.val)) # commented out for now, but tests for stationarity
  mutate(summer_data = future_map(data, ~ dplyr::filter(., between(.$month, 5, 9))),
         gc_er_gpp = future_map(summer_data, ~ grangertest(diff(.$ER) ~ diff(.$GPP), 
                                                   order = 1)),
         gc_gpp_chla = future_map(data, ~grangertest(.$GPP ~ .$chla, 
                                                     order = 1))) %>%
  unnest(c(gc_er_gpp, gc_gpp_chla), names_sep = ".") %>%
  mutate(regime = if_else(year < 2012, 0, 1))

# plotting dataframe
df_gc_p <- df_gc %>%
  select(year, regime, contains("Pr")) %>%
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
  scale_color_aaas(name = "regime",
                    breaks = c(0,1),
                    labels = c("phytoplankton", "macrophytes")) +
  guides(color = FALSE) +
  annotate(geom = "rect", xmin = 2011,
            xmax = 2012,
            ymin = -Inf,
            ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
  annotate("segment", x = 2007.5, xend = 2011, y = 0.15, yend = 0.15, 
           colour = "black", arrow=arrow(length = unit(0.2,"cm"))) +
  annotate("segment", x = 1996.5, xend = 1993, y = 0.15, yend = 0.15, 
           colour = "black", arrow=arrow(length = unit(0.2,"cm"))) +
  annotate(geom = "text",
           x = 2002,
           y = 0.15,
           label = "GPP and ER tightly coupled",
           size = 2.5) +
  geom_line() +
  theme(legend.position = c(0.2, 0.75),
        legend.spacing.y = unit(0.2, 'cm')) + 
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1993, 2018, 2),
                     limits = c(1993, 2018)) +
  xlab("") +
  ylab(expression(Granger~causality~p-val)) +
  labs(subtitle = "A")
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
         regime = if_else(year < 2012, 0, 1)) %>%
  ggplot(aes(x = year,
             y = NEP_mean,
             color = as.factor(regime),
             group = 1)) +
  geom_line() +
  geom_point() +
  annotate(geom = "rect", xmin = 2011,
           xmax = 2012,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "dark blue") +
  geom_errorbar(aes(ymin = lower.ci.NEP, ymax = upper.ci.NEP), width =0.2)  +
  geom_hline(yintercept = 0) +
  scale_color_aaas(name = "regime",
                   breaks = c(0,1),
                   labels = c("phytoplankton", "macrophytes")) +
  scale_x_continuous(breaks = seq(1993, 2018, 2),
                     limits = c(1993, 2018)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * -250*44*180*12/32/44/1e3, 
                                         name = expression("Summer C efflux to atmosphere (kg C "*km^{-1}*")"))) +
  theme(legend.position = c(0.2, 0.15)) + 
  # scale_color_viridis_c(name = "year") +
  # theme(legend.key.height = unit(0.2, "cm"),
  #       legend.direction = "horizontal",
  #       legend.position = c(0.5, 0.85),
  #       legend.background = element_rect(color = "transparent")) +
  labs(subtitle = "B") +
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))
p_nep


# Third panel, Damkohler number -------------------------------------------
# First, estimate annual ratio of macrophytes to chla
df_ratio <- df_wq %>%
  filter(solute == "CHLA",
         between(month(date), 7, 9),
         year(date) > 1992) %>% #want period of highest chlorophyll
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(chla = mean(value, na.rm = TRUE)) %>%
  left_join(df_mac) %>%
  mutate(area = if_else(is.na(area), 0, area)) %>%
  # mutate_at(vars(area) # linear interpolate for 2015
  mutate(ratio = area / chla) %>%
  mutate_at(vars(ratio), rescale, to = c(0.001, 1)) %>%
  select(year, ratio)

# join data together to calculate damkohler
df_da <- left_join(df_met, df_q) %>%
  left_join(pivot_wider(df_wq, names_from = solute, values_from = value)) %>%
  mutate(uptake_alg = GPP * (12/32) / 8, # calculate autotrophic nitrogen uptake based on stoichiometry
         uptake_mac = GPP * (12/32) / 20) %>%  #C/N = 8:1 for algae, 20:1 for macro
  # mutate(period = if_else(year < 2006, 0, 1)) %>%
  left_join(df_ratio) %>%
  mutate(uptake = uptake_alg * (1 - ratio) + uptake_mac * ratio) 

# Summarize this data by year
df_dasum <- df_da %>%
  mutate(DOC = if_else(is.na(DOC), mean(DOC, na.rm = TRUE), DOC), #replace NA DOC with mean DOC
         POC = 32 * (CHLA/100 + PheoP/100), #conversion of pigments to POC from Minaudo et al. 2012
         TOC = POC + DOC, #total organic carbon
         rxnC = abs((ER)) * 12/32, #reaction in units of gC/d/m2
         transC = discharge.daily * ((TOC) * 86400) / 200, #transport in gC/d/m2 assuming 1 m depth and 200 m wide
         DaC = rxnC / transC) %>%
  mutate(NO3 = NO3 * 14.01 / 62,
         NH4 = NH4 * 14.01 / 18,
         # NO3 = if_else(is.na(NO3), mean(NO3, na.rm = TRUE), NO3), #replace NA NO3 with mean NO3
         # TKN = if_else(is.na(TKN), mean(TKN, na.rm = TRUE), TKN), #replace NA TKN with mean TKN
         TN = NO3 + NH4,
         rxnN = abs(uptake), #reaction in units of gN/d/m2
         transN = discharge.daily * ((TN) * 86400) / 200, #transport in gN/d/m2 assuming 1 m depth and 200 m wide
         DaN = rxnN / transN) %>%
  group_by(year) %>%
  summarize(DaC = mean(DaC, na.rm = TRUE),
            DaN = mean(DaN, na.rm = TRUE)) %>%
  mutate(regime = if_else(year < 2012, 0, 1)) %>%
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
  # scale_linetype_manual(name = "element",
  #                         breaks = c("C", "N"),
  #                         values = c("solid", "long dash")) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_continuous(breaks = seq(1993, 2018, 2)) +
  scale_color_aaas(name = "regime",
                   breaks = c(0,1),
                   labels = c("phytoplankton", "macrophytes")) +
  guides(color = FALSE) +
  annotation_logticks(
    base = 10,
    sides = "l") +
  theme(legend.position = c(0.18, 0.4),
        legend.spacing.y = unit(0.2, 'cm')) +
  # annotate(geom = "rect", xmin = 2011,
  #          xmax = 2013,
  #          ymin = min(df_dasum$Da),
  #          ymax = max(df_dasum$Da),
  #          alpha = 0.4,
  #          fill = "dark blue") +
  # annotate(geom = "text",
  #          x = 2006,
  #          y = 10^-7.6,
  #          label = "ER:TOC river flux",
  #          size = 2.5) +
  # annotate(geom = "text",
  #          x = 2000.5,
  #          y = 10^-6.1,
  #          label = "autotrophic nitrate uptake:TN river flux",
  #          size = 2.5) +
  ylab(expression("mean annual Damkohler")) +
  theme(axis.title.x = element_blank()) +
  labs(subtitle = "C")
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
  scale_fill_aaas(name = "regime",
                  breaks = c(0,1),
                  labels = c("phytoplankton", "macrophytes")) +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("environmental driver") +
  ylab("linear response slope of GPP") +
  labs(subtitle = "D")
  

p_cont
# 
# 
# # Cumulative annual discharge vs cumlative annual gpp
# df_all <- left_join(df_met, df_q)
# 
# df_cum <- df_all %>%
#   mutate(month = month(date),
#          year = year(date)) %>%
#   # filter(between(month, 6, 7)) %>%
#   group_by(year) %>%
#   summarize(q_cum = sum(ifelse(
#     between(month, 3, 5), discharge.daily, NA), na.rm = TRUE),
#     GPP_cum = sum(ifelse(
#       between(month, 4, 10), GPP, NA),  na.rm = TRUE)) %>%
#   mutate(transition = if_else(year < 2012, "before", "after"))
# 
# ggplot(data = df_cum,
#        aes(x = q_cum,
#            y = GPP_cum,
#            color = year,
#            shape = transition)) +
#   geom_point() +
#   stat_smooth(aes(group = transition), method = "lm") +
#   scale_color_viridis_c() +
#   theme_bw() +
#   geom_text(aes(label = year))
# 
# mod <- lm(GPP_cum ~ q_cum:transition, data = df_cum)
# plot(mod)
# plot(residuals(mod))
# summary(mod)
# Final figure ------------------------------------------------------------

# Plot all plots
((p_grang/ p_da) | (p_nep / p_cont)) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure2_v3.svg",
         device = "svg",
         dpi = 300,
         height = 100,
         width = 183,
         units = "mm")

