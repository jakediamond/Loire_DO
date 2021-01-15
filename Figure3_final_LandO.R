# 
# Purpose: To plot Figure 2 for the paper
# Author: Jake Diamond
# Date: April 8, 2020
# 

# Set working directory
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("Z:/Loire_DO")
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
theme_set(theme_bw(base_size=9)+
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
  ungroup()

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Load middle loire water quality data
df_wq <- readRDS("Data/Loire_DO/middle_loire_wq5") %>%
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
# First panel, change in controls on GPP ---------------------------------
# Read in regression data
df_reg <- read_csv2("Data/data_for_regression_v2.csv") %>%
  # mutate(period = if_else(period == "pre", 0, 1))
  mutate(period = if_else(year(date) < 2013, 0, 1)) %>%
  select(date, GPP, med_light, discharge, period)

# Set seed
set.seed(69)

# Scaled data
df_reg_scale <- df_reg %>%
  # mutate_at(3:4, scale) %>%
  dplyr::filter(between(month(date), 5, 9)) %>%
  drop_na() %>%
  group_by(period) %>%
  slice_sample(n = 200) %>%
  ungroup() %>%
  # dplyr::select(-date) %>%
  as.data.frame()

# Interactions
fit_int <- lm(GPP ~ (discharge +med_light) *period, data = df_reg_scale)
summary(fit_int)
plot(fit_int)
fit_scale <- summ(fit_int, scale = TRUE, vifs = TRUE)
tidy(x)
# effect_plot(fit_int, pred = med_light, interval = TRUE, plot.points = TRUE)
# export_summs(fit_int, scale = TRUE, vifs = TRUE,
#              error_pos = "right", ci_level = 0.95,
#              statistics = "all",
#              to.file = "xlsx", file.name = "Data/Loire_DO/mult_reg_discharge_light.xlsx")
# anova(fit_ints, fit_int2)
# library(interactions)
# interact_plot(fit_int, pred = discharge, modx = period)

# Make tidy
df_cont <- tidy(fit_scale) %>%
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
# Second panel, Damkohler number -------------------------------------------
# join data together to calculate damkohler; use monthly averages
df_da <- left_join(df_met, df_q) %>%
  mutate(depth = 0.134 * discharge.daily^0.4125) %>% #get depth from discharge data, site specific eqn
  group_by(year, month) %>%
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE),
            q = mean(discharge.daily, na.rm = TRUE),
            depth = mean(depth, na.rm = TRUE)) %>%
  na_kalman() %>%
  right_join(pivot_wider(df_wq, names_from = solute, values_from = value) %>%
               dplyr::select(date, CHLA, PheoP) %>%
               mutate(month = month(date),
                      year = year(date))) %>%
  mutate(uptake_alg = GPP * (12/32) / 8, # calculate autotrophic nitrogen uptake based on stoichiometry
         uptake_mac = GPP * (12/32) / 20) %>%  #C/N = 8:1 for algae, 20:1 for macro
  filter(month == 8) %>%
  right_join(read_excel("Data/Da_DOC_TIN.xlsx")) %>%
  mutate(uptake = if_else(year < 2005, uptake_alg, uptake_mac),
         POC = 32 * (CHLA + PheoP)/1000) %>% #conversion of pigments to POC from Minaudo et al. 2016
  # na_kalman() %>% #interpolate remaining NAs
  mutate(TOC = POC + DOC)

df_da %>%
  mutate(period = if_else(year <2005, 0, 1)) %>%
  group_by(period) %>%
  summarize(PO = mean(POC),
            Ps = sd(POC, na.rm = T),
            TO = mean(TOC),
            Ts = sd(TOC),
            TI = mean(TIN),
            TIs = sd(TIN),
            up = mean(uptake),
            ups = sd(uptake))

# Calculate dahmkohler number by monthly averages
df_dasum <- df_da %>%
  mutate(rxnC = abs(ER) * 12/32 * 10000 * 250, #reaction in units of gC/d over 10 km reach
         transC = ((q * TOC) * 86400), #transport in gC/d 
         DaC = rxnC / transC) %>%
  mutate(rxnN = abs(uptake) * 10000 * 250, #reaction in units of gN/d
         transN = ((q * TIN) * 86400), #transport in gN/d/m2 assuming 250 m wide
         DaN = rxnN / transN) %>%
  dplyr::select(year, DaC, DaN) %>%
  mutate(regime = if_else(year < 2013, 0, 1)) %>%
  ungroup()

lm(DaC ~ year, data = df_dasum)
lm(DaN ~ year, data = filter(df_dasum, year < 2005))

p_da <- df_dasum %>%
  pivot_longer(cols = -c(year, regime), 
               names_to = "element", 
               names_prefix = "Da",
               values_to = "Da") %>%
  ggplot(aes(x = year,
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
  scale_x_continuous(breaks = seq(1993, 2018, 3)) +
  scale_y_continuous(limits = c(0, 2.3),
                     expand = c(0,0)) +
  scale_color_aaas(name = "metabolic regime",
                   breaks = c(0,1),
                   labels = c("phytoplankton", "macrophytes")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  annotate(geom = "text",
           x = 2009,
           y = 1.1,
           label = "transport limited",
           size = 2.5) +
  annotate(geom = "text",
           x = 2009,
           y = 0.9,
           label = "reaction limited",
           size = 2.5) +
  annotate(geom = "rect", xmin = 2013,
           xmax = 2014,
           ymin = 0,
           ymax = 2.3,
           alpha = 0.4,
           fill = "dark blue") +
  annotate(geom = "text",
           x = 2011,
           y = 0.6,
           label = "ER:TOC river flux",
           size = 2.5) +
  annotate(geom = "text",
           x = 2003,
           y = 2,
           label = "autotrophic nitrate uptake:TIN river flux",
           size = 2.5) +
  ylab(expression("August Damkohler number (-)")) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.7, 0.7),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.height = unit(0.2, "cm"),
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = -0.1, unit='cm'))
p_da

lm()
df_da %>%
  select(year, TOC, TIN, POC, DOC, uptake, ER) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value)) + 
  geom_point() + facet_wrap(~name, scales  ="free")

# third panel, NEP over time ---------------------------------------------
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
  scale_x_continuous(breaks = seq(1993, 2018, 3),
                     limits = c(1993, 2018)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * -250*44*180*12/32/44/1e3, 
                                         name = expression("summer C efflux (Mg C "*km^{-1}*")"))) +
  theme(legend.position = c(0.2, 0.18),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.key.height = unit(0.2, "cm"),
        legend.margin = margin(t = 0, b = -0.1, unit='cm')) + 
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))
p_nep
# Fourth panel, Granger causality ------------------------------------------
# Nest data
df_met_n <- df_met %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER,
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
                                                    order = 1))) %>%
  unnest(c(gc_er_gpp), names_sep = ".") %>%
  mutate(regime = if_else(year < 2013, 0, 1))

# plotting dataframe
df_gc_p <- df_gc %>%
  dplyr::select(year, regime, contains("Pr")) %>%
  ungroup() %>%
  drop_na() %>%
  pivot_longer(cols = -c(year, regime), 
               names_to = "causality", 
               values_to = "pval") %>%
  mutate(causality = case_when(grepl("er", causality) ~ "ER ~ GPP"))

p_grang <- ggplot(data = df_gc_p,
                  aes(x = year,
                      y = pval,
                      color = as.factor(regime),
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
  scale_x_continuous(breaks = seq(1993, 2018, 3),
                     limits = c(1993, 2018)) +
  xlab("") +
  ylab(expression(Granger~causality~p-val))
p_grang

# Final figure ------------------------------------------------------------

# Plot all plots
(((p_cont / p_nep) | (p_da / p_grang)) +
   plot_annotation(tag_levels = 'a'))
ggsave(filename = "Figures/Figure3_LandO_final.svg",
         device = "svg",
         dpi = 300,
         height = 100,
         width = 183,
         units = "mm")

