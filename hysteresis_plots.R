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
library(ggrepel)
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
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER)
df_wq <- readRDS("Data/Loire_DO/middle_loire_wq")
df_wq_wide <- df_wq %>%
  pivot_wider(names_from = solute, values_from = value)
df_combine <- left_join(df_met, df_wq, by = "date")
df_mac <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>%
  summarize(sa = sum(surface_area, na.rm = TRUE))
df_cor <- read_xlsx("Data/Loire_DO/corbicula.xlsx") %>%
  filter(site %in% c("Belleville")) %>%
  group_by(year) %>%
  summarize(density = mean(density, na.rm = TRUE))
df_lowflows <- readRDS("Data/Discharge/low_flow_duration")

# Summarize solute data by year
df_all <- df_wq_wide %>%
  filter(between(month, 4, 9),
         year > 1979) %>%
  group_by(year) %>%
  summarize(mp = mean(PO4, na.rm = TRUE),
            mc = mean(CHLA, na.rm = TRUE),
            ms = mean(SPM, na.rm = TRUE),
            ses = sd(SPM, na.rm = TRUE) / sqrt(n()),
            sep = sd(PO4, na.rm = TRUE) / sqrt(n()),
            sec = sd(CHLA, na.rm = TRUE) / sqrt(n())) %>%
  left_join(df_mac) %>%
  left_join(df_cor) %>%
  left_join(df_met %>%
              filter(between(month, 4, 9)) %>%
              group_by(year) %>%
              summarize(gpp = median(GPP, na.rm = TRUE),
                        seg = sd(GPP, na.rm = TRUE) / sqrt(n()))
            ) %>%
  left_join(df_lowflows) %>%
  mutate(veg = if_else(is.na(sa), "no_veg", "veg"),
         brk = if_else(year > 2005, "after", "before"),
         max_lf = max(lf_dur, na.rm = T)) 

summary(lm(scale(df_all$gpp)~scale(df_all$mc)))
ggplot(df_all, aes(x = mc, y = gpp )) +geom_text_repel(aes(label = year))+
  geom_path(aes(color = year, group = 1))
# Hysteresis plots --------------------------------------------------------
# summer average plot for phosphate and chlorophyll
phos_chl_plot <- ggplot(df_all,
                        aes(x = mp, y = mc, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c(name = "Year") +
  theme(legend.position = c(0.85, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent")) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_chl_plot

# summer average plot for phosphate and turbidity
phos_tur_plot <- ggplot(df_all,
                        aes(x = mp, y = ms, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  # geom_line(data = df_mod, aes(x = x, y = y), linetype = "dashed") +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean summer TSS (mg "~L^{-1}*")")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_tur_plot

# state space corbicula and po4
phos_cor_plot <- ggplot(df_all,
                        aes(x = mp, y = density, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  # geom_errorbar(aes(ymin = mp - sep, ymax = mp + sep)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  ylab(expression(italic(Corbicula )~sp.*" (ind "*m^{-2}*")")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))
phos_cor_plot

# state space corbicula and chla
chl_cor_plot <- ggplot(df_all,
                        aes(x = density, y = mc, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  # geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  geom_errorbar(aes(ymin = mc - sec, ymax = mc + sec)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent")) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  xlab(expression(italic(Corbicula )~sp.*" (ind "*m^{-2}*")")) +
  ylab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")"))
chl_cor_plot

# state space gpp and chla
chl_gpp_plot <- ggplot(df_all,
                       aes(x = mc, y = gpp * lf_dur / max_lf, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  # geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
  # geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c() +
  guides(color = FALSE,
         fill = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent")) +
  scale_x_log10() + 
  # scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean gpp ("*mu*g~L^{-1}*")")) +
  xlab(expression("Mean summer chlorophyll-a ("*mu*g~L^{-1}*")"))
chl_gpp_plot

# state space gpp and p
p_gpp_plot <- ggplot(df_all,
                       aes(x = mp, y = gpp * lf_dur / max_lf, color = year, group = 1)) + 
  geom_path(size = 1.5) +
  geom_text_repel(aes(label = year)) +
  geom_point(aes(fill = veg), shape = 21, size = 1.5) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  # geom_errorbarh(aes(xmin = mc - sec, xmax = mc + sec)) +
  # geom_errorbar(aes(ymin = gpp - seg, ymax = gpp + seg)) +
  scale_color_viridis_c() +
  guides(color = FALSE,
         fill = FALSE) +
  theme(legend.position = c(0.2, 0.4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(color = "transparent")) +
  scale_x_log10() + 
  # scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean gpp ("*mu*g~L^{-1}*")")) +
  xlab(expression("Mean summer phosphorus (mg"*~L^{-1}*")"))
p_gpp_plot

((phos_chl_plot | chl_cor_plot | chl_gpp_plot) / 
    (phos_cor_plot | phos_tur_plot | p_gpp_plot)) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure3_hysteresis_new.tiff",
         device = "tiff",
         dpi = 300,
         height = 120,
         width = 183,
         units = "mm")

df_all %>%
  mutate(SD = exp(0.88-0.26*log(mc)-0.334*log(ms))) %>%
  ggplot(.,
         aes(x = mp, y = SD, color = year, group = 1)) + 
  geom_path(size = 1.5) + 
  geom_point(aes(fill = veg), shape = 21, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(name = "Macrophytes",
                    breaks = c("no_veg", "veg"),
                    labels = c("absent", "present"),
                    values = c("white", "black")) +
  geom_errorbarh(aes(xmin = mp - sep, xmax = mp + sep)) +
  # geom_errorbar(aes(ymin = ms - ses, ymax = ms + ses)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks() +
  ylab(expression("Mean summer SD)")) +
  xlab(expression("Mean summer"~PO[4]^{`3-`}-P~"(mg "~L^{-1}*")"))

# arrow = arrow(type = "closed", length = unit(0.15, "cm"))
# 
# 
# Try to fit a monod model re Ibelwings et al 2007
data_nls <- df_wq_wide %>%
  filter(between(month, 4, 9)) %>%
  select(SPM, PO4, CHLA, TP) %>%
  drop_na()
mmModel <- nls(SPM ~ Vm * PO4 / (K + PO4), 
               data = data_nls,
               start=list(Vm=200, K=5))
summary(mmModel)
coef(mmModel) 
confint(mmModel)
x <- seq(min(data_nls$PO4), max(data_nls$PO4), length=100)
y <- predict(mmModel, list(PO4=x))
plot(data_nls$PO4, data_nls$SPM, las=1, pch=16)
points(x, y, type='l', col='blue')

df_mod <- tibble(x= x, y=y)
# 
# 
# 
# 
library(plotly)
df_use <- left_join(df_all, df_mac)
plot_ly(data =df_use,
        x=~mp, y=~mc, z=~sa, type="scatter3d", 
        mode="lines", line = list(width = 6, color = ~year, reverscale = FALSE,
                                  colorscale = 'Viridis'))




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
  
