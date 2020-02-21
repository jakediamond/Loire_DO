# 
# Purpose: To plot metabolism results for Loire
# Author: Jake Diamond
# Date: January 13, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("D:/jake.diamond/Loire_DO")

# Load libraries
# library(streamMetabolizer)
library(lubridate)
library(patchwork)
library(ggrepel)
library(tidyverse)

# Set the plotting theme
theme_set(theme_bw(base_size=15)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm"))))

# Loading data and cleaning -----------------------------------------------
# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Get into long format and prepare for plotting
df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  left_join(df_q) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  pivot_longer(cols = c(-date), names_to = "key", values_to = "value") %>%
  mutate(type_plot = recode(key,
                            `ER` = "ER~(g~O[2]~m^{-2}~d^{-1})",
                            `GPP` = "GPP~(g~O[2]~m^{-2}~d^{-1})",
                            `NEP` = "NEP~(g~O[2]~m^{-2}~d^{-1})",
                            `K600.daily` = "k[600]~(d^{-1})",
                            `discharge.daily` = "Discharge~(m^3~s^{-1})"),
         color = if_else(key == "NEP" & value > 0, 
                         "dark green", 
                         "black"))

df_met_l$type_plot <- factor(df_met_l$type_plot,
                             levels = c("GPP~(g~O[2]~m^{-2}~d^{-1})",
                                        "ER~(g~O[2]~m^{-2}~d^{-1})",
                                        "NEP~(g~O[2]~m^{-2}~d^{-1})",
                                        "k[600]~(d^{-1})",
                                        "Discharge~(m^3~s^{-1})"))

# Metabolism summary (summer months)
metab_summary <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  filter(between(month, 5, 10)) %>%
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

# Plotting ----------------------------------------------------------------
# Plot the time series data
met_ts_plot <- ggplot(data = df_met_l,
                      aes(x = date,
                          y = value)) +
  geom_point(aes(color = I(color)), alpha = 0.3, size = 0.15,
             show.legend = FALSE) +
  facet_grid(rows = vars(type_plot), scales = "free_y",
             labeller = label_parsed) + 
  scale_x_date(date_breaks = "1 years",
               limits = c(ymd("1993-01-01"),
                          ymd("2018-12-31")),
               date_labels = "%Y") +
  xlab("") +
  ylab("")
met_ts_plot

# Plot the metabolism fingerprint (summertime)
fingerprint <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER)) %>%
  left_join(df_q) %>%
  ungroup() %>%
  filter(between(month, 4, 9)) %>%
  ggplot(aes(x = GPP, y = ER) ) +
  # stat_contour(aes(z = ..ndensity..)) +
  # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha =0.3)  +
  # stat_density_2d(aes(fill = (1-..nlevel..)),
  #                 geom = "polygon", alpha =0.2, bins = 5) +
  stat_density_2d(geom = "contour",
                  aes(alpha = ..nlevel..)) +
  facet_wrap(~year) +
  # geom_vline(xintercept = 1) +
  geom_abline(slope = -1, intercept = 0) +
  # stat_density_2d(aes(fill = stat(piece)),
  #                 geom = "polygon", 
  #                 n = 100, 
  #                 bins = 10, 
  #                 contour = T) + 
  # scale_alpha_continuous(limits = c(0.4, 0.4),
  #                        values = c(NA, NA, NA, 1, NA)) +
  # geom_density_2d(aes(fill = ..level.., color = year), bins = 3) +
  # scale_fill_viridis_c(limits = c(0.001, 1)) +
  scale_x_continuous(limits = c(0, 20)) +
  # scale_y_log10(limits = c(0, 500))
  scale_y_continuous(limits = c(-20, 0))
fingerprint
ggsave(plot = fingerprint, 
       filename = "Figures/Middle_Loire/fingerprint_facets_april_sept.tiff",
       device = "tiff",
       units = "mm",
       width = 260,
       height = 183)

# Add the metabolism centroids
fp_cent <- fingerprint +
  geom_point(data = metab_summary, aes(x = GPP_mean,
                                       y = ER_mean,
                                       color = year),
             size = 0.9,
             show.legend = FALSE) +
  # geom_path(data = x, aes(x = GPP_mean,
  #                          y = ER_mean,
  #                          color = year)) +
  geom_text_repel(data = metab_summary, aes(x = GPP_mean,
                                            y = ER_mean,
                                            label = year,
                                            color = year),
                  show.legend = FALSE, 
                  size = 1.8) +
  # geom_errorbar(aes(ymax = upper.ci.ER,
  #                   ymin = lower.ci.ER)) +
  # geom_errorbarh(aes(xmax = upper.ci.GPP,
  #                    xmin = lower.ci.GPP)) +
  guides(color = FALSE) +
  scale_color_viridis_c() +
  scale_alpha_continuous(name = "density") +
  geom_abline(slope = -1, intercept = 0) +
  xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
  ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
  theme(legend.key.height = unit(0.2, "cm"),
        # legend.direction = "horizontal",
        legend.position = c(0.18, 0.20),
        legend.background = element_rect(color = "transparent"))
fp_cent
# NEP time series plot
nep <- df_met %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  ungroup() %>%
  # filter(between(month, 4, 9)) %>%
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
  scale_x_continuous(breaks = seq(1995, 2020, 5),
               limits = c(1993, 2020)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * -60000*200*44*365*12/32/44/1e9, 
                                         name = "Annual C efflux to atmosphere (Gg C)")) +
  scale_color_viridis_c(name = "year") +
  theme(legend.key.height = unit(0.2, "cm"),
        legend.direction = "horizontal",
        legend.position = c(0.28, 0.12),
        legend.background = element_rect(color = "transparent")) +
  xlab("")+
  ylab(expression(NEP~(g~O[2]~m^{-2}~d^{-1})))
nep

((met_ts_plot / (fp_cent + nep)) + plot_layout(heights = c(3, 1),
                                        widths = c(1, 1))) %>%
  ggsave(filename = "Figures/Middle_Loire/Figure3.svg",
         device = "svg",
         dpi = 300,
         height = 160,
         width = 183,
         units = "mm")

# get specific contours ---------------------------------------------------
# Testing something
a <- df_met_l %>%
  select(-type_plot) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  drop_na() %>%
  filter(between(month(date), 4, 10))
h <- c(MASS::bandwidth.nrd(a$GPP), MASS::bandwidth.nrd(a$ER))
  
dens <- MASS::kde2d(
  a$GPP, a$ER, h = h, n = 100,
  lims = c(0, 20, -20, 0)
)
zdf <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
breaks <- pretty(range(zdf$z), 10)

z <- tapply(zdf$z, zdf[c("x", "y")], identity)

cl <- grDevices::contourLines(
  x = sort(unique(dens$x)), y = sort(unique(dens$y)), z = dens$z,
  levels = breaks
)
library(sp)
SpatialPolygons(
  lapply(1:length(cl), function(idx) {
    Polygons(
      srl = list(Polygon(
        matrix(c(cl[[idx]]$x, cl[[idx]]$y), nrow=length(cl[[idx]]$x), byrow=FALSE)
      )),
      ID = idx
    )
  })
) -> cont

coordinates(a) <- ~GPP+ER

data_frame(
  ct = sapply(over(cont, geometry(a), returnList = TRUE), length),
  id = 1:length(ct),
  lvl = sapply(cl, function(x) x$level)
) %>% 
  count(lvl, wt=ct) %>% 
  mutate(
    pct = n/length(a),
    pct_lab = sprintf("%s of the points fall within this level", scales::percent(pct))
  )

# get the kde2d information: 
mv.kde <- MASS::kde2d(a$GPP, a$ER, n = 100,
                      lims = c(0, 20, -20, 0))
dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
dy <- diff(mv.kde$y[1:2])
sz <- sort(mv.kde$z)
c1 <- cumsum(sz) * dx * dy

# specify desired contour levels:
prob <- c(0.20,0.40,0.60, 0.80)

# plot:
dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
dc <- melt(mv.kde$z)
dc$prob <- approx(sz,1-c1,dc$value)$y
p <- ggplot(dc,aes(x=Var1,y=Var2))+
  geom_contour(aes(z=prob,color=..level..),breaks=prob)
print(p)
fingerprint

# extract results
get_contour <- function(kd_out, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>%
    mutate(prob = prob)
}



library(broom)
library(furrr)
## density function
kd <- a %>%
  mutate(year = year(date)) %>%
  select(year, GPP, ER) %>%
  group_by(year) %>%
  nest() %>%
  mutate(kd = future_map(data, ~ks::kde(., compute.cont=TRUE, h=0.2)),
         tkd = future_map(kd, tidy))

# small function to get kd data for plotting
kd_fun <- function(kdout, prob = "50%") {
  # tkd = tidy(kdout) %>%
  #   pivot_wider(names_from = variable, values_from = value) %>%
  #   rename(GPP = x1, ER = x2)
  contour_95 = with(kdout, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
  # kd_conts = stack(pluck(kdout, 10)) %>%
  #   rename(estimate = values, cont = ind)
  # result = left_join(tkd, kd_conts, by = "estimate")
}
kd2 <- a %>%
  mutate(year = year(date)) %>%
  select(year, GPP, ER) %>%
  group_by(year) %>%
  nest() %>%
  mutate(kd = future_map(data, ~ks::kde(., compute.cont=TRUE, h=0.2)),
         res = future_map(kd, ~kd_fun(., prob = "5%"))) %>%
  select(-kd, -data) %>%
  unnest(res) %>%
  mutate(alph = if_else(year < 2000, 0.1,
                        if_else(between(year, 2000, 2005),
                                0.25,
                                if_else(between(year, 2006, 2012),
                                        0.5,
                                        0.8))),
         ) %>%
  ungroup()

  #   b <- kd %>% 
  #     select(year, tkd) %>% 
  #     unnest(tkd) %>%
  #     pivot_wider(names_from = variable, values_from = value) %>%
  #     rename(GPP = x1, ER = x2)
  # 
  # dat_out <- kd %>%
  #   mutate(out = map2(kd, c("20%"), ~get_contour(.x, .y))) %>% 
  #   group_by(prob) %>% 
  #   mutate(n_val = 1:n()) %>% 
  #   ungroup()
  
  str(kd2)
  (ggplot(data=kd2, aes(x, y)) +
    # geom_tile(aes(fill=level)) +
    # geom_point(data = d, alpha = I(0.4), size = I(0.4), colour = I("yellow")) +
    geom_path(aes(x, y, color = year, alpha = alph,
                  size = as.factor(year))) +
    # geom_polygon(aes(x, y, group = year, fill = year), alpha = 0.1) +
    # geom_text(aes(label = prob), data = 
    #             filter(dat_out, (prob%in% c("10%", "20%","80%") & n_val==1) | (prob%in% c("90%") & n_val==20)),
    #           colour = I("black"), size =I(3))+
    scale_color_viridis_c()+
      scale_size_manual(name = "year",
                        breaks = seq(1993, 2018, 1),
                        values = seq(3, 0.5, -0.1)) +
    scale_fill_viridis_c()+
    theme_bw() +
      guides(size = FALSE, alpha = FALSE) +
    # geom_point(data = x, aes(x = GPP_mean,
    #                          y = ER_mean,
    #                          color = year)) +
    # geom_path(data = x, aes(x = GPP_mean,
    #                         y = ER_mean,
    #                         color = year)) +
    # geom_text_repel(data = x, aes(x = GPP_mean,
    #                               y = ER_mean,
    #                               label = year,
    #                               color = year)) +
    # geom_errorbar(aes(ymax = upper.ci.ER,
    #                   ymin = lower.ci.ER)) +
    # geom_errorbarh(aes(xmax = upper.ci.GPP,
    #                    xmin = lower.ci.GPP)) +
    # guides(color = FALSE) +
    # scale_color_viridis_c(option = "magma") +
    geom_abline(slope = -1, intercept = 0) +
    theme_bw(base_size=7) +
    xlab(expression(GPP~(g~O[2]~m^{-2}~d^{-1})))+
    ylab(expression(ER~(g~O[2]~m^{-2}~d^{-1}))) +
    scale_x_continuous(limits = c(0, 20)) +
    scale_y_continuous(limits = c(-20, 0))) %>%
  ggsave(filename = "Figures/Middle_Loire/shifting_fingerprint_v2.tiff",
         device = "tiff",
         dpi = 300,
         height = 140,
         width = 183,
         units = "mm")
  
  
  
  sdf
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  set.seed(1001)
  
  ## data
  d <- MASS::mvrnorm(1000, c(0, 0.2), matrix(c(1, 0.4, 1, 0.4), ncol=2)) %>% 
    magrittr::set_colnames(c("x", "y")) %>% 
    as_tibble() 
  d <- select(a, x = GPP, y = ER)
  ## density function
  kd <- ks::kde(d, compute.cont=TRUE, h=0.2)
  
  ## extract results
  get_contour <- function(kd_out=kd, prob="5%") {
    contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                            z=estimate, levels=cont[prob])[[1]])
    as_tibble(contour_95) %>% 
      mutate(prob = prob)
  }
  
  dat_out <- map_dfr(c("5%", "20%","40%", "60%"), ~get_contour(kd, .)) %>% 
    group_by(prob) %>% 
    mutate(n_val = 1:n()) %>% 
    ungroup()
  
  ## clean kde output
  kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
    mutate(estimate = c(kd$estimate %>% t))
  
  ggplot(data=kd_df, aes(x, y)) +
    # geom_raster(aes(fill=estimate), interpolate = TRUE) +
    # geom_point(data = d, alpha = I(0.4), size = I(0.4), colour = I("yellow")) +
    geom_path(aes(x, y, group = prob), 
              data=dat_out) +
    geom_text(aes(label = prob), data = 
                filter(dat_out, (prob %in% c("10%", "20%","80%") & n_val==50) | 
                         (prob%in% c("90%") & n_val==20)),
              colour = I("black"), size =I(3))+
    scale_fill_viridis_c()+
    theme_bw() +
    theme(legend.position = "none")
fingerprint  
