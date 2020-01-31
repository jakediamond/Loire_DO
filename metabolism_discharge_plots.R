# 
# Purpose: To plot discharge and metabolism together across years
# Author: Jake Diamond
# Date: January 8, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(patchwork)
library(tidyverse)

# Load long term DO and metabolism data
df_do <- readRDS("Data/all_DO_cleaned")
# df_met <- readRDS("Data/Loire_DO/metabolism_results_all_years_constrainedK")
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")
# Load discharge data
df_q <- readRDS("Data/dampierre_discharge_daily")

# Combine data
df <- df_do %>%
  filter(site == "dampierre") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(DO_max = max(DO, na.rm = TRUE),
            DO_min = min(DO, na.rm = TRUE)) %>%
  mutate(DO_max = ifelse(is.infinite(DO_max), NA, DO_max),
         DO_min = ifelse(is.infinite(DO_min), NA, DO_min)) %>%
  select(date, DO_max, DO_min) %>%
  right_join(df_met %>%
               mutate(ER = ifelse(ER < 0, -ER, NA),
                      GPP = ifelse(GPP < 0, NA, GPP))) %>%
  ungroup()

# Discharge/metabolism example plots ---------------------------------------------
# year to plot
for(i in 1993:2018){
  yr <- i
  # define limits to axes
  ylim.prim <- c(0, 20)   
  ylim.sec <- c(0, 2000)
  
  # Calculate the plot variables for the axes
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- b*(ylim.prim[1] - ylim.sec[1])
  # Constant part of each graph
  p_const <- ggplot() + geom_line(data = filter(df_q, between(date,
                                                   ymd(paste0(yr,"-01-01")),
                                                   ymd(paste0(yr,"-12-31")))), 
                       aes(x = date,
                           y = a + discharge.daily * b),
                       color = "blue") +
    scale_x_date(date_breaks = "1 month",
                 limits = c(ymd(paste0(yr,"-01-01")),
                            ymd(paste0(yr,"-12-31"))),
                 date_labels = "%m") +
    theme_bw(base_size=7) +
    theme(panel.grid.minor = element_blank(),
          axis.line.y.right = element_line(color = "blue"), 
          axis.ticks.y.right = element_line(color = "blue"),
          axis.text.y.right = element_text(color = "blue"), 
          axis.title.y.right = element_text(color = "blue")) +
    scale_y_continuous(limits = c(0, 20),
                       breaks = seq(0, 20, 5),
                       sec.axis = sec_axis(~ (. - a) / b, 
                                           name = expression("Mean daily discharge ("*m^3~s^{-1}*")")
                       )
    ) +
    xlab("month") +
    ggtitle(label = yr)
  
  # DO graph
  p_DO <- p_const +
    geom_line(data = filter(df, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
              aes(x = date,
                  y = DO_max,
                  linetype = "max"),
              color = "black") +
    geom_line(data = filter(df, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
              aes(x = date,
                  y = DO_min,
                  linetype = "min"),
              color = "black") +
    ylab(expression(DO~(mg~L^{`-1`}))) +
    scale_linetype_manual(
      name   = '',
      breaks = c('max', 'min'),
      values = c("solid", "dashed"),
      labels = c('max', 'min')) +
    theme(legend.position = c(0.15, 0.85),
          legend.background = element_rect(colour = NA, fill = NA),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_blank())
  
  # GPP graph
  p_GPP <- p_const +
    geom_line(data = filter(df, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
              aes(x = date,
                  y = GPP),
              color = "black") +
    ylab(expression(GPP~(g~O[2]~m^{`-2`}~d^{`-1`})))
  
  # ER graph
  p_ER <- p_const +
    geom_line(data = filter(df, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
              aes(x = date,
                  y = ER),
              color = "black") +
    ylab(expression(ER~(g~O[2]~m^{`-2`}~d^{`-1`})))
  
  # k graph
  # # redefine limits to axes
  ylim.prim <- c(0, 8)   
  ylim.sec <- c(0, 2000)
  
  # Calculate the plot variables for the axes
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- b*(ylim.prim[1] - ylim.sec[1])
  # graph
  p_k <- p_const +
    geom_line(data = filter(df, between(date,
                                        ymd(paste0(yr,"-01-01")),
                                        ymd(paste0(yr,"-12-31")))), 
              aes(x = date,
                  y = K600.daily),
              color = "black") +
    ylab(expression(K[600]~(d^{`-1`})))
  # Save plot
  ((p_DO + p_k) / (p_GPP + p_ER)) %>%
    ggsave(filename = paste0("Figures/Middle_Loire/metabolism_discharge_new", yr, ".png"),
           device = "png",
           dpi = 300,
           width = 18,
           height = 18,
           units = "cm")
}









