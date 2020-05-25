# 
# Purpose: To plot Figure X, metabolism summaries, for middle Loire River
# Author: Jake Diamond
# Date: January 13, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)
library(patchwork)
library(ggpmisc)
library(ggsci)

# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  gather(key, value, -date) %>%
  mutate(type_plot = recode(key,
                            `ER` = "ER~(g~O[2]~m^{-2}~d^{-1})",
                            `GPP` = "GPP~(g~O[2]~m^{-2}~d^{-1})",
                            `NEP` = "NEP~(g~O[2]~m^{-2}~d^{-1})",
                            `K600.daily` = "k[600]~(d^{-1})"))

df_met_l$type_plot <- factor(df_met_l$type_plot,
                             levels = c("GPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "ER~(g~O[2]~m^{-2}~d^{-1})",
                                        "NEP~(g~O[2]~m^{-2}~d^{-1})",
                                        "k[600]~(d^{-1})"))

# Plot the data
(ggplot(data = df_met_l,
        aes(x = date,
            y = value)) +
    geom_point(alpha = 0.4, size = 0.8) +
    facet_grid(rows = vars(type_plot), scales = "free_y",
               labeller = label_parsed) + 
    scale_x_date(date_breaks = "1 years",
                 limits = c(ymd("1993-01-01"),
                            ymd("2018-12-31")),
                 date_labels = "%Y") +
    theme_bw(base_size=7) +
    theme(panel.grid.minor = element_blank()) + 
    xlab("") +
    ylab("")) %>%
  ggsave(filename = "Figures/Middle_Loire/supplementary/metabolism_pool_facets.png",
         device = "png",
         dpi = 300,
         width = 18.4,
         height = 9.2,
         units = "cm")

# Plot of ER vs K600
(ggplot(data = df_met,
        aes(x = K600.daily,
            y = ER,
            color = month(date))) +
    geom_point() +
    scale_color_viridis_c(name = "month", alpha = 0.5) +
    ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
    xlab(expression(k[600]~(d^{-1}))) +
    theme(legend.position = c(0.75, 0.12),
          legend.direction = "horizontal")
    ) %>%
  ggsave(filename = "Figures/Middle_Loire/supplementary/ER_K.png",
         device = "png",
         dpi = 300,
         width = 9.2,
         height = 9.2,
         units = "cm")

# Summarize missing values
df_met %>% 
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise_all(~sum(is.na(.)))

# Total N over the years
(read_xlsx("Data/Loire_DO/Ntot.xlsx", sheet = 1) %>%
    select(year = Annee, TKN, NO3 = `N-NO3`) %>%
    pivot_longer(cols = c(TKN, NO3), names_to = "solute") %>%
    ggplot(data = .,
           aes(x = year,
               y = value,
               fill = solute)) +
    geom_col() +
    scale_fill_aaas() +
    scale_y_continuous(limits = c(0, 5)) +
    ylab("summer nitrogen (mg N/L)") +
    xlab("") +
    theme(legend.position = c(0.2, 0.8))) %>%
  ggsave(filename = "Figures/Middle_Loire/supplementary/summer_nitrogen_breakdown_bar_new.png",
         width = 10,
         height = 10,
         units = "cm",
         device = "png",
         dpi = 300)


# Summer mean ER vs GPP
(df_met %>%
    mutate(month = month(date),
           year = year(date),
           GPP = ifelse(GPP < 0, NA, GPP),
           ER = ifelse(ER > 0, NA, ER),
           NPP = GPP + ER) %>%
    ungroup() %>%
    filter(between(month, 5, 9)) %>%
    group_by(year) %>%
    summarize(GPP_mean = mean(GPP, na.rm = TRUE),
              GPP_sd = sd(GPP, na.rm = TRUE),
              GPP_n= n(),
              ER_mean = mean(ER, na.rm = TRUE),
              ER_sd = sd(ER, na.rm = TRUE),
              ER_n= n()) %>%
    mutate(GPP_se = GPP_sd / sqrt(GPP_n),
           lower.ci.GPP = GPP_mean - qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
           upper.ci.GPP = GPP_mean + qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
           ER_se = ER_sd / sqrt(ER_n),
           lower.ci.ER = ER_mean - qt(1 - (0.05 / 2), ER_n - 1) * ER_se,
           upper.ci.ER = ER_mean + qt(1 - (0.05 / 2), ER_n - 1) * ER_se) %>%
    ggplot(aes(x = GPP_mean,
               y = ER_mean,
               color = year)) +
    geom_point() +
    geom_text_repel(aes(label = year)) +
    # geom_errorbar(aes(ymax = upper.ci.ER,
    #                   ymin = lower.ci.ER)) +
    # geom_errorbarh(aes(xmax = upper.ci.GPP,
    #                    xmin = lower.ci.GPP)) +
    guides(color = FALSE) +
    geom_abline(slope = -1, intercept = 0) +
    theme_bw(base_size=7) +
    theme() +
    xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
    ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
    ggtitle(label = "Summer mean ER vs. GPP")) %>%
  ggsave(filename = "Figures/Middle_Loire/summer_ER_vs_GPP_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 9,
         units = "cm")


fingerprint <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 4, 10)) %>%
  ggplot(aes(x=GPP, y=ER) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha =0.3)  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white", alpha =0.3) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(-20, 0)) +
  theme(
    legend.position='none'
  )
x <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 5, 9)) %>%
  group_by(year) %>%
  summarize(GPP_mean = mean(GPP, na.rm = TRUE),
            GPP_sd = sd(GPP, na.rm = TRUE),
            GPP_n= n(),
            ER_mean = mean(ER, na.rm = TRUE),
            ER_sd = sd(ER, na.rm = TRUE),
            ER_n= n()) %>%
  mutate(GPP_se = GPP_sd / sqrt(GPP_n),
         lower.ci.GPP = GPP_mean - qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
         upper.ci.GPP = GPP_mean + qt(1 - (0.05 / 2), GPP_n - 1) * GPP_se,
         ER_se = ER_sd / sqrt(ER_n),
         lower.ci.ER = ER_mean - qt(1 - (0.05 / 2), ER_n - 1) * ER_se,
         upper.ci.ER = ER_mean + qt(1 - (0.05 / 2), ER_n - 1) * ER_se)

(fingerprint + 
  geom_point(data = x, aes(x = GPP_mean,
                 y = ER_mean,
                 color = year)) +
  geom_text_repel(data = x, aes(x = GPP_mean,
                                y = ER_mean,
                                label = year,
                                color = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  scale_color_viridis_c(option = "magma") +
  geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "metab_fingerprint.png",
         dpi = 300,
         width = 6,
         height = 6,
         units = 'in')  
fingerprint + x



nep <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 4, 9)) %>%
  group_by(year) %>%
  summarize(NEP_mean = mean(NEP, na.rm = TRUE),
            NEP_sd = sd(GPP, na.rm = TRUE),
            NEP_n= n()) %>%
  mutate(NEP_se = NEP_sd / sqrt(NEP_n),
         lower.ci.NEP = NEP_mean - qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se,
         upper.ci.NEP = NEP_mean + qt(1 - (0.05 / 2), NEP_n - 1) * NEP_se) %>%
  ggplot(aes(x = year,
             y = NEP_mean,
             color = year)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower.ci.NEP, ymax = upper.ci.NEP), width =0.2)  +
    geom_hline(yintercept = 0) +
  scale_color_viridis_c(option = "magma") +
  # geom_text_repel(aes(label = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  # geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  theme() +
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))


((fp / nep) +plot_layout(heights = c(3, 1),
                       widths = c(1, 1))) %>%
  ggsave(filename = "nep_witherrors.png",
         device = "png",
         dpi = 300,
         height = 6,
         width = 7.25,
         units = "in")















(df_met %>%
    ungroup() %>%
    # dplyr::select(-time_frame) %>%
    mutate(GPP = ifelse(GPP < 0, NA, GPP),
           ER = ifelse(ER > 0, NA, ER),
           NPP = GPP + ER,
           year = year(date),
           month = month(date)) %>%
    filter(between(month, 4, 10)) %>%
    ggplot() +
    theme_bw(base_size = 6) +
    theme(panel.grid.minor = element_blank()) +
    stat_summary(aes(x = year, y = GPP),
                 fun.y = mean, geom = "point") +
    # stat_summary(aes(x = year, y = GPP),
    #              fun.y = median, geom = "point", color = "red") +
    stat_summary(aes(x = year, y = GPP),
                 fun.data = mean_cl_boot, geom = "errorbar") + 
    # geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #             aes(x = year, y = GPP), method = "lm",
    #             alpha = 0.5) +
    # stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #              formula = y~x, 
    #              aes(x = year, y = GPP, 
    #                  label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #              parse = TRUE,
    #              eq.with.lhs = "italic(GPP[summer])~`=`~",
    #              label.x = 0.8, label.y = 0.55,
    #              size = 2) +
    stat_summary(aes(x = year, y = ER),
                 fun.y = mean, geom = "point") +
    # stat_summary(aes(x = year, y = ER),
    #              fun.y = median, geom = "point", color = "red") +
    stat_summary(aes(x = year, y = ER),
                 fun.data = mean_cl_boot, geom = "errorbar") +
    # geom_smooth(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #             aes(x = year, y = ER), method = "lm",
    #             alpha = 0.5) +
    # stat_poly_eq(data = filter(df_summary, !(year %in% c(2000, 2001, 2007, 2008))), 
    #              formula = y~x, 
    #              aes(x = year, y = ER, 
    #                  label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #              parse = TRUE,
    #              eq.with.lhs = "italic(ER[summer])~`=`~",
    #              label.x = 0.8, label.y = 0.02,
    #              size = 2) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(1994, 2018, 2)) +
    xlab("") +
    ylab(expression(flux~(g~O[2]~m^{-2}~d^{-1})))) %>%
  ggsave(filename = "Figures/Middle_Loire/annual_flux_change_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")

ggplot(data = filter(df_mid_clean, solute == "CHLA"),
       aes(x = date, y = value)) + geom_point()





fp <- fingerprint + 
  geom_point(data = x, aes(x = GPP_mean,
                           y = ER_mean,
                           color = year)) +
  geom_text_repel(data = x, aes(x = GPP_mean,
                                y = ER_mean,
                                label = year,
                                color = year)) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  scale_color_viridis_c(option = "magma") +
  geom_abline(slope = -1, intercept = 0) +
  theme_bw(base_size=7) +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1})))
