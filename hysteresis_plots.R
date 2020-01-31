
# Purpose: To plot Figure X, hysteresis state plots, for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(furrr)
library(patchwork)
library(xts)
library(readxl)
library(changepoint)
library(imputeTS)
library(tidyverse)

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load some data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")
df_all <- left_join(df_met, df_mid_clean, by = "date")
df_mac <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>%
  summarize(sa = sum(surface_area, na.rm = TRUE))

# Summarize solute data by year
df_solutes_summary <- df_mid_wide %>%
  filter(between(month, 4, 9),
         year > 1978) %>%
  group_by(year) %>%
  summarize(mp = mean(PO4, na.rm = TRUE),
            mc = mean(CHLA, na.rm = TRUE),
            sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
            sec = sd(CHLA, na.rm = TRUE) / sqrt(n()))
# Hysteresis plots --------------------------------------------------------
# summer average plot
(ggplot(df_solutes_summary,
        aes(x = mp, y = mc, color = year, group = 1)) + 
   geom_path(size = 2) + 
   geom_point() +
   geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
   geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
   scale_color_viridis_c(name = "Year") +
   theme(legend.position = c(0.8, 0.4)) +
   scale_x_log10() + scale_y_log10() +
   ylab(expression("Mean summer chlorophyll-a (mg "~L^{-1}*")")) +
   xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))) %>%
  ggsave(filename = "Figures/Middle_Loire/chla_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)
# arrow = arrow(type = "closed", length = unit(0.15, "cm"))
# 
library(plotly)
df_use <- left_join(df_solutes_summary, df_mac)
plot_ly(data =df_use,
        x=~mp, y=~mc, z=~sa, type="scatter3d", 
        mode="lines", line = list(width = 6, color = ~year, reverscale = FALSE,
                                  colorscale = 'Viridis'))
# Turbidity vs nutrients
(ggplot(df_mid_wide %>%
          filter(between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mp = mean(PO4, na.rm = TRUE),
                    ms = mean(SPM, na.rm = TRUE),
                    sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
                    ses = sd(SPM, na.rm = TRUE) / sqrt(n())), 
        aes(x = mp, y = ms, color = year)) + 
    geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
    geom_point() + 
    geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
    geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.8, 0.4)) +
    scale_x_log10() + scale_y_log10() +
    ylab(expression("Mean summer TSS (mg "~L^{-1}*")")) +
    xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))) %>%
  ggsave(filename = "Figures/Middle_Loire/turb_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)




df_cam <- read_excel("Data/Camille_phd/high_freq_data.xlsx")
ggplot(df_cam %>%
         filter(between(month(Date), 4, 10),
                site == "cm"),
       aes(x = `P-PO43- (µg/L)`, y = `Chla (µg/L)`, color = year(Date))) + 
  geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  # scale_x_continuous(limits = c(0,2)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_mid_ss,
       aes(x = PO4, y = CHLA, color = year)) + 
  geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  # scale_x_continuous(limits = c(0,2)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_all %>%
         mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
         filter(solute == "CHLA",
                between(month, 6, 8)),
       aes(x = value, y = GPP, color = year)) + 
  # geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) + 
  geom_point() + 
  scale_color_viridis_c() +
  scale_x_log10() 

ggplot(df_all %>%
         mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
         filter(solute == "CHLA",
                between(month, 5, 10)),
       aes(x = value, y = GPP, color = year)) + 
  # geom_path( arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  stat_summary(fun.y = mean, geom = "point") +
  # stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  scale_color_viridis_c() +
  scale_x_log10() 

(ggplot(df_all  %>%
          mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
          filter(solute == "CHLA",
                 between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mgpp = mean(GPP, na.rm = TRUE),
                    mc = mean(value, na.rm = TRUE),
                    segpp = sd(GPP, na.rm = TRUE) / sqrt(n()),
                    sec = sd(value, na.rm = TRUE) / sqrt(n())), 
        aes(x = mc, y = mgpp, color = year, group = 1)) + 
    geom_path(size = 2) + 
    geom_point() + 
    # geom_errorbar(aes(ymin = mgpp - segpp, ymax = mgpp + segpp)) +
    # geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.85, 0.25)) +
    scale_y_log10() +
    xlab(expression("Mean summer chlorophyll-a (mg "~L^{-1}*")")) +
    ylab(expression("Mean summer GPP"~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/gpp_chl_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)

(ggplot(df_all  %>%
          mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
          filter(solute == "PO4",
                 between(month, 4, 10),
                 year > 1978) %>%
          group_by(year) %>%
          summarize(mgpp = mean(GPP, na.rm = TRUE),
                    mp = mean(value, na.rm = TRUE),
                    segpp = sd(GPP, na.rm = TRUE) / sqrt(n()),
                    sep = sd(value, na.rm = TRUE) / sqrt(n())), 
        aes(x = mp, y = mgpp, color = year)) + 
    geom_path( arrow = arrow(type = "closed", length = unit(0.15, "cm"))) + 
    geom_point() + 
    geom_errorbar(aes(ymin = mgpp - segpp, ymax = mgpp + segpp)) +
    geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
    scale_color_viridis_c(name = "Year") +
    theme(legend.position = c(0.85, 0.25)) +
    scale_y_log10() +
    xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")")) +
    ylab(expression("Mean summer GPP"~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/gpp_p_sum_hyst.png",
         device = "png",
         height = 9,
         width = 9,
         units = "cm",
         dpi = 300)


ggplot(df_solutes %>%
         filter(solute == "PO[4]^{`3-`}-P",
                between(month(date), 4, 10)) %>%
         mutate(dx = month_mean - lag(month_mean),
                dt = abs(month - lag(month)),
                dot = dx/dt), 
       aes(x = month_mean, y = dot, color = date)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0,0.5)) +
  scale_x_log10() + scale_y_log10()

ggplot(df_solutes %>%
         filter(solute == "Chlorophyll~a",
                between(month(date), 4, 10)) %>%
         mutate(dx = month_mean - lag(month_mean),
                dt = abs(month - lag(month)),
                dot = dx/dt), 
       aes(x = month_mean, y = dot, color = date)) + 
  geom_point() 

library(moments)
df_solutes %>%
  ungroup() %>%
  filter(between(month, 6, 9)) %>%
  arrange(solute, date) %>%
  group_by(solute) %>%
  nest() %>%
  mutate(sk = map(data, ~rollapply(.$month_mean, width=36,
                          FUN=function(x)
                            skewness(x, na.rm=TRUE), by=1,
                          by.column=TRUE, partial=TRUE,
                          fill=NA, align="right"))) %>%
  unnest(cols = c(sk, data)) %>%
  ggplot() +
  geom_line(aes(x = date, y = sk)) +
  facet_wrap(~solute, scales = "free_y")


df_solutes %>%
  ungroup() %>%
  filter(between(month, 4, 9)) %>%
  arrange(solute, date) %>%
  group_by(solute) %>%
  nest() %>%
  mutate(sk = map(data, ~rollapply(.$month_mean, width=24,
                                   FUN=function(x)
                                     var(x, na.rm=TRUE), by=1,
                                   by.column=TRUE, partial=TRUE,
                                   fill=NA, align="right"))) %>%
  unnest(cols = c(sk, data)) %>%
  ggplot() +
  geom_line(aes(x = date, y = sk)) +
  facet_wrap(~solute, scales = "free_y")




# Granger
df_met_n <- df_all %>%
  ungroup() %>%
  filter(solute == "CHLA") %>%
  # dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER,
         year = year(date),
         month = month(date)) %>%
  # filter(between(month, 5, 8)) %>%
  group_by(year) %>%
  nest()
library(furrr)
library(lmtest)
# granger causality
df_gc <- df_met_n %>%
  mutate(gc_er_gpp = future_map(data, ~grangertest(diff(.$GPP) ~ diff(.$value), 
                                                   order = 1))) %>%
  unnest(gc_er_gpp)

ggplot(data = na.omit(df_gc),
        aes(x = year,
            y = `Pr(>F)`)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 7) +
    scale_x_continuous(breaks = seq(1994, 2018, 2)) +
    xlab("") +
    ylab(expression(ER[summer]*`~`*GPP[summer]~Granger~causality~pval))
  
  ggsave(filename = "Figures/Middle_Loire/granger_causality_er_gpp_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")
  
