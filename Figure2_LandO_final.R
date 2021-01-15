# 
# Purpose: To plot supplementary material plots
# Author: Jake Diamond
# Date: January 13, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(scales)
library(patchwork)
library(tidyverse)

# Load data ---------------------------------------------------------
# solute data
df_solutes <- readRDS("Data/Loire_DO/middle_loire_wq5")

# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Macrophytes 
df_macros <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                       sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>%
  summarize(sa = sum(surface_area, na.rm = TRUE))

# Corbicula
df_corb <- read_xlsx("Data/Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  dplyr::select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(density = mean(dens, na.rm = TRUE),
            se = sd(dens, na.rm = TRUE) / sqrt(n()))

df_lowflows <- readRDS("Data/Discharge/low_flow_duration")

# Phase space plots --------------------------------------------------------
# Load all data
df_wq <- df_solutes %>%
  pivot_wider(names_from = solute, values_from = value)
# Combine wq and met data
# df_combine <- left_join(df_met, df_wq, by = "date")

# Summarize all data by year and join
df_all <- df_wq %>%
  filter(between(month, 4, 10),
         year > 1979) %>%
  group_by(year) %>%
  summarize(mp = mean(PO4, na.rm = TRUE),
            mc = mean(CHLA, na.rm = TRUE),
            ms = mean(SPM, na.rm = TRUE),
            ses = sd(SPM, na.rm = TRUE) / sqrt(n()),
            sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
            sec = sd(CHLA, na.rm = TRUE) / sqrt(n())) %>%
  left_join(df_macros) %>%
  left_join(df_corb) %>%
  left_join(df_met %>%
              mutate(year = year(date),
                     month = month(date)) %>%
              filter(between(month, 4, 10)) %>%
              group_by(year) %>%
              summarize(gpp = mean(GPP, na.rm = TRUE),
                        seg = sd(GPP, na.rm = TRUE) / sqrt(n()))
  ) %>%
  left_join(df_lowflows) %>%
  mutate(veg = if_else(!is.na(sa) | year == 2015, "veg", "no_veg"),
         brk = if_else(year > 2005, "after", "before"))
         # max_lf = max(lf_dur, na.rm = T))

# Plotting ----------------------------------------------------------------
# Overall theme
# # Set the plotting theme
theme_set(theme_bw(base_size=9)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.tag.position = c(0.27,0.95)))

# summer average plot for phosphate and chlorophyll
phos_chl_plot <- ggplot(df_all,
                        aes(x = mp, y = mc, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 2.2, show.legend = FALSE) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c(name = "year") +
  theme(legend.position = c(0.85, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent"),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10(breaks = c(0.01, 0.1),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  ylab(expression("mean summer chlorophyll"~italic(a)~"("*mu*g~L^{-1}*")")) +
  xlab(expression("mean summer"~PO[4]-P~"(mg"~L^{-1}*")"))
phos_chl_plot

# summer average plot for phosphate and turbidity
phos_tur_plot <- ggplot(df_all,
                        aes(x = mp, y = ms, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 2.2, show.legend = FALSE) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10(breaks = c(0.01, 0.1),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = c(1, 10, 100),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  theme(plot.tag.position = c(0.25,0.95)) +
  ylab(expression("mean summer TSS (mg"~L^{-1}*")")) +
  xlab(expression("mean summer"~PO[4]-P~"(mg "~L^{-1}*")"))
phos_tur_plot

# state space corbicula and po4
phos_cor_plot <- ggplot(df_all,
                        aes(x = density, y = mp, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 2.2, show.legend = FALSE) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = density - se, xmax = density + se)) +
  geom_errorbar(aes(ymin = mp - sep, ymax = mp + sep)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks = c(0.01, 0.1),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  theme(plot.tag.position = c(0.25,0.95)) +
  xlab(expression(italic(Corbicula~fluminea)~"(ind"~m^{-2}*")")) +
  ylab(expression("mean summer"~PO[4]-P~"(mg"~L^{-1}*")"))
phos_cor_plot

# state space corbicula and chla
chl_cor_plot <- ggplot(df_all,
                       aes(x = density, y = mc, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 2.2) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = density - se, xmax = density + se)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  theme(legend.position = c(0.25, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent")) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  xlab(expression(italic(Corbicula~fluminea)~"(ind"~m^{-2}*")")) +
  ylab(expression("mean summer chlorophyll"~italic(a)~"("*mu*g~L^{-1}*")"))
chl_cor_plot

# state space gpp and chla
chl_gpp_plot <- ggplot(df_all,
                       aes(x = mc, y = gpp, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_point(aes(fill = veg), shape = 21, size = 2.2) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
  geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c() +
  guides(color = FALSE,
         fill = FALSE) +
  theme(legend.position = c(0.11, 0.7),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank()) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "b") +
  ylab(expression("mean summer GPP ("*g~O[2]~m^{-2}~d^{-1}*")")) +
  xlab(expression("mean summer chlorophyll"~italic(a)~"("*mu*g~L^{-1}*")"))
chl_gpp_plot

# state space gpp and p
p_gpp_plot <- ggplot(df_all,
                     aes(x = mp, y = gpp, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_point(aes(fill = veg), shape = 21, size = 2.2) +
  scale_fill_manual(name = "macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c() +
  guides(color = FALSE,
         fill = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent"),
        plot.tag.position = c(0.25,0.95)) +
  scale_x_log10(breaks = c(0.01, 0.1),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "b") +
  ylab(expression("mean summer GPP ("*g~O[2]~m^{-2}~d^{-1}*")")) +
  xlab(expression("mean summer"~PO[4]-P~"(mg"~L^{-1}*")"))
p_gpp_plot

((phos_chl_plot / phos_tur_plot) | (chl_cor_plot / phos_cor_plot) | (chl_gpp_plot / p_gpp_plot)) +
    plot_annotation(tag_levels = "a")
  ggsave(filename = "Figures/Figure2_L&O_final.svg",
         device = "svg",
         dpi = 300,
         height = 120,
         width = 183,
         units = "mm")
     