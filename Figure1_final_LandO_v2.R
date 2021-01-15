# 
# Purpose: To plot Figure 1, time series changepoints for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(ggsci)
library(patchwork)
library(tidyverse)

# Set seed
set.seed(42)

# # Set the plotting theme
theme_set(theme_bw(base_size=9)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
                  plot.tag.position = c(0.1,0.95)))

# Loading data and cleaning -----------------------------------------------
# Load long term wq data
df <- readRDS("Data/Loire_DO/all_longterm_wq_data") %>%
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
              # dplyr::select(-`Conductiv.20°C`, -cd_opecont) %>%
              mutate(date = as.Date(date),  
                     site_no = str_pad(site_no, 8, pad = "0"),
                     Annee = year(date),
                     Mois = month(date),
                     Jour = day(date)))

# Bit of cleaning
df <- df %>%
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
df_mid_wq <- filter(df, site_no %in% c("04048000", #Dampierre
                                       "04050500" #Orleans
                                       )) %>%
  distinct(site_no, date, .keep_all = TRUE)

# Load long term DO data
df_do <- readRDS("Data/all_DO_cleaned") %>%
  group_by(site) %>%
  distinct(datetime, .keep_all = TRUE) %>%
  arrange(datetime) %>%
  ungroup() %>%
  filter(site == "dampierre") %>%
  left_join(readRDS("Data/all_DO_data") %>%
              dplyr::filter(var == "T",
                            site == "dampierre") %>%
              select(datetime, temp = value),
            by = "datetime") %>%
  left_join(readRDS("Data/Loire_DO/dampierre_temp_estimates"), by = "datetime") %>%
  mutate(temp = ifelse(is.na(temp), temp.water, temp)) %>%
  select(-date, -temp.water) %>%
  mutate(DO_sat = ifelse(temp <= 0,
                      0,
                      14.652 - 0.41022 * temp + 0.007991 * 
                        temp^2 - 0.000077774 * temp^3),
         DO_per = DO_use / DO_sat)

# Load corbicula data
df_cor <- read_xlsx("Data/Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2)

# Load metab data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")

# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Get metabolism into long format and prepare for plotting (filter 4 bad days not caught earlier)
df_met_l <- df_met %>%
  ungroup() %>%
  select(-NPP) %>%
  left_join(df_q) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) %>%
  pivot_longer(cols = c(-date), names_to = "key", values_to = "value") %>%
  mutate(value = case_when(between(date, ymd("2001-12-15"), ymd("2001-12-18"))~ 0,
                           TRUE ~ value)) %>%
  mutate(type_plot = dplyr::recode(key,
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

# Load macrophyte data and simplify
df_mac <- read_xlsx("Data/Macrophytes/all_macrophytes.xlsx",
                    sheet = 1) %>%
  group_by(year) %>%
  filter(species != "Elodea nuttallii") %>% #outlier species
  add_tally(surface_area) %>%
  mutate(per = surface_area / n) %>% #only want to name dominant species in legend
  mutate(species2 = ifelse(per < 0.11 | species %in% c("Ludwigia grandiflora",
                                                       "Najas marina",
                                                       "Elodea canadensis",
                                                       "Potamogeton nodosus",
                                                       "Potamogeton pectinatus",
                                                       "Spirodela polyrhiza",
                                                       "Lemna minor",
                                                       "Potamogeton perfoliatus"),
                           "other", species),
         type2 = ifelse(type == "floating_submerged", "submerged", type))
df_mac$species2 <- factor(df_mac$species2,
                       levels = c("Ranunculus fluitans", "Myriophyllum spicatum",
                                       "Vallisneria spiralis", "other"))

load("Figures/Figure1_R_environment_data_final_v2.RData")
# Figure 1 main plot -----------------------------------------------------------
# Reorder solutes and name for plotting
levels(df_solutes$solute) <- c("TSS",
                               "PO[4]-P",
                               "BOD[5]",
                               "NO[3]-N",
                               "Chlorophyll~italic(a)")

# Labels for solute plot
solute_labels <- df_solutes %>%
  filter(!is.na(solute)) %>%
  group_by(solute) %>%
  mutate(lab_date = ymd("1995-01-01"), lab_value = max(month_mean, na.rm = TRUE)) %>%
  distinct(solute, lab_date, lab_value)

# Rename changepoints for plotting
levels(solute_bp_out$solute) <- c("TSS",
                                  "PO[4]-P",
                                  "BOD[5]",
                                  "NO[3]-N",
                                  "Chlorophyll~italic(a)")

# Same for values of the breakpoints
levels(solute_bp_values$solute) <- c("TSS",
                                     "PO[4]-P",
                                     "BOD[5]",
                                     "NO[3]-N",
                                     "Chlorophyll~italic(a)")

# Get x scale for all plots
brks <- seq(ymd("1990-01-01"), ymd("2019-01-01"), "1 year")
lbls <- format(seq(ymd("1990-01-01"), ymd("2019-01-01"), "5 years"), "%Y")
labs_yrs <- c(sapply(lbls, function(x) {
  c(x, rep("", 4))
  }))

# Macrophyte plot
p_mac <- ggplot(data = filter(df_mac, species != "Elodea nuttallii", year < 2019),
                aes(x = year,
                    y = (surface_area / 7200)*100)) + #per meter square over 7200 m2
  geom_bar(aes(fill = species2), stat = "identity") +
  stat_summary(aes(linetype = type2), fun.y = "sum", geom = "line") + 
  scale_x_continuous(limits = c(1993, 2019),
                     breaks = seq(1993, 2018, 1),
                     expand = expand_scale(add = c(0.15, 0))) +
  geom_vline(aes(xintercept = 2009),
             linetype = "longdash",
             color = "dark green",
             size = 0.5,
             alpha = 0.8) +
  annotate(geom = "text", label = "2009", x = 2010, y = 0.12, 
           color = "dark green", size = 2.2) +
  annotate(geom = "text", x = 2015, y = 2.6, 
           label = "no data", color = "black",
           angle = 90, vjust = 0.2) +
  scale_fill_manual(name = "species", values = c("#a6cee3", "#1f78b4",
                                                 "#b2df8a", "#33a02c")) +
  scale_linetype_discrete(name = "macrophyte type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.24, 0.66),
        legend.key.height = unit(0.2, "cm"),
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = -0.1, unit='cm'),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  xlab("") +
  ylab(expression("macrophyte cover (%)"))
p_mac

# Plot corbicula data
p_c <- ggplot(data = df_cor_sum,
              aes(x = date,
                  y = density)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = density - se,
                    ymax = density + se)) +
  geom_vline(aes(xintercept = ymd("2001-06-01")),
             linetype = "longdash",
             color = "black",
             size = 0.5,
             alpha = 0.8) +
  annotate(geom = "rect",
           xmin = ymd("2001-01-01"),
                xmax = ymd("2002-01-01"),
                ymin = 10^-2,
                ymax = 10^4,
            fill = "black",
            alpha = 0.4) +
  annotate(geom = "segment",
           x = ymd("1993-01-01"),
           xend = ymd("2018-12-31"),
           y = 1/(250*7500*1.4e-7/120),
           yend = 1/(250*7500*1.4e-7/120),
           color = "black",
           linetype = "dotted") +
  annotate(geom = "text",
           x = ymd("2002-07-01"),
           y = 2500,
           color = "black",
           label = "2001",
           size = 3) +
  annotate(geom = "text",
           x = ymd("2008-01-01"),
           y = 920,
           color = "black",
           label = "approximate 100% daily turnover",
           size = 3) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  scale_y_log10(sec.axis = sec_axis(~ . *250*7500*1.4e-7/120, #m3/s filtration in 7.5 km reach divided by median summer discharge 
                                         name = "turnover ratio (-)",
                                    labels = trans_format("log10", math_format(10^.x)),
                                    breaks = c(10^-4, 10^-3, 10^-2, 10^-1,
                                               10^0, 10^1)),
                labels = trans_format("log10", math_format(10^.x)),
                breaks = c(10^-1, 10^0, 10^1, 10^2,
                           10^3),
                limits = c(10^-2, 10^4),
                expand = c(0, 0)) +
  annotation_logticks(sides="l") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        legend.position = c(0.2, 0.6),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  # labs(subtitle = "C") +
  xlab("") +
  ylab(expression(italic(Corbicula)~"(ind "*m^{-2}*")"))
p_c

# Add breakpoints to solutes
# Plot of monthly solute time series
p_solutes <- ggplot(data = df_solutes,
                     aes(color = solute,
                         fill = solute)) +
  geom_line(aes(x = date,
                y = month_mean,
                group = 1,
                alpha = summer)) +
  geom_vline(data = solute_bp_out %>%
               dplyr::select(solute, mean), 
             aes(xintercept = mean),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8,
             color = "black") +
  geom_rect(data = solute_bp_out,
            aes(xmin = lower, 
                xmax = upper, 
                ymin = -Inf, 
                ymax = Inf),
            alpha = 0.4) +
  geom_text(data = solute_bp_out,
            aes(x = mean + years(1),
                y = lab_value * 0.95,
                label = year(mean)),
            size = 3) +
  geom_text(data = solute_labels,
            aes(label = solute,
                x = lab_date,
                y = lab_value), 
            vjust = 1, 
            size = 3,
            parse = TRUE) +
  facet_wrap(~solute, scales = "free_y",
             labeller = label_parsed,
             ncol = 1) +
  scale_alpha_manual(name = "",
                     breaks = c("summer", "winter"),
                     values = c(1, 0.2)) +
  scale_color_manual(values = c("#EE0000FF", "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  scale_fill_manual(values = c("#EE0000FF",  "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  # labs(subtitle = "A") +
  xlab("") +
  ylab(expression("concentration (mg "~L^{-1}*")"))
p_solutes

# Plot of metabolism data
p_met <- ggplot() + 
  geom_line(data = filter(df_met_l, key %in% c("GPP", "ER")),
             aes(x = date,
                 y = value,
                 color = key,
                 group = key), alpha = 0.8, size = 0.25,
            show.legend = FALSE) +
  scale_color_manual(name = "",
                     # labels = parse_format(),
                     values = c("blue", "dark blue")) +
  scale_x_date(limits = c(ymd("1993-01-01"), ymd("2018-12-31")),
               breaks = brks,
               labels = labs_yrs) +
  scale_y_continuous(limits = c(-26, 26)) +
  geom_hline(yintercept = 0) +
  geom_vline(data = filter(met_bp_out, key == "GPP") %>%
               dplyr::select(key, mean), 
             aes(xintercept = mean,
                 color = key),
             linetype = "longdash",
             size = 0.5,
             alpha = 0.8,
             show.legend = FALSE) +
  geom_rect(data = filter(met_bp_out, key == "GPP"),
            aes(xmin = lower,
                xmax = upper,
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.4) +
  geom_text(data = filter(met_bp_values, period == 2),
            aes(x = ymd("2015-01-01"),
                y = mean_flux * 3.4,
                label = "2014",
                color = key),
            size = 3,
            show.legend = FALSE) +
  annotate(geom = "text",
           x = ymd("1999-01-01"),
           y = 24,
           color = "darkblue",
           label = "GPP",
           size =3) +
  annotate(geom = "text",
           x = ymd("1999-01-01"),
           y = -24,
           color = "blue",
           label = "ER",
           size = 3) +
  theme(axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # legend.position = c(0.15, 0.15),
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha('white', 0.4))) +
  # labs(subtitle = "D") +
  xlab("") +
  ylab(expression("metabolic flux"~(g~O[2]~m^{-2}~d^{-1})))
p_met

# Magnitude change plots --------------------------------------------------
# Solute magnitude change
p_mag_sol <- ggplot(data = solute_bp_values,
                    aes(x = as.factor(period),
                        y = ml,
                        color = solute)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = ml - qt(1 - (0.05 / 2), (sdl/sel)^2 - 1) * sel, 
                    ymax = ml + qt(1 - (0.05 / 2), (sdl/sel)^2 - 1) * sel),
                width = 0.1) +
  geom_blank(data = df_solutes %>% 
               select(solute, month_mean) %>%
               group_by(solute) %>%
               summarize(max = max(month_mean, na.rm = TRUE)) %>%
               right_join(solute_bp_values), 
             aes(y=max)) +
  geom_text(data = solute_bp_values,
            aes(x = period,
                y = ml * 1.8,
                color = solute,
                label = paste0("list(",
                               signif(ml, 1),
                               "%+-%",
                               signif(sdl, 1),
                               ")")),
            parse = TRUE,
            size = 3) +
  facet_wrap(~solute, ncol = 1, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("#EE0000FF", "#631879FF", "#008280FF", "#BB0021FF", "#008B45FF")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))
p_mag_sol

# Metabolism change
p_mag_met <- ggplot(data = met_bp_values,
                    aes(x = as.factor(period),
                        y = mean_flux,
                        color = key)) +
  geom_blank(data = tibble(key_f = c("GPP", "ER", "GPP", "ER"), 
                           fake_value = c(0, 0, 26, -26)) %>%
               mutate(key_f = factor(.$key_f, 
                      levels=c('GPP','ER'))) %>%
               right_join(met_bp_values), 
             aes(y=fake_value)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = mean_flux - qt(1 - (0.05 / 2), (sd_flux/se_flux)^2 - 1) * se_flux, 
                    ymax = mean_flux + qt(1 - (0.05 / 2), (sd_flux/se_flux)^2 - 1) * se_flux),
                width = 0.1) +
  geom_text(data = met_bp_values,
            aes(x = period,
                y = mean_flux * 1.5,
                color = key, 
                label = paste0("list(",
                               signif(mean_flux, 2),
                               "%+-%",
                               signif(sd_flux, 2),
                               ")")),
            parse = TRUE,
            size = 3) +
  facet_wrap(~key_f, ncol = 1, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("blue", "darkblue")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))
p_mag_met

# Corbicula change
p_mag_cor <- ggplot(data = cor_bp_values,
                    aes(x = as.factor(period),
                        y = mean)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = mean - qt(1 - (0.05 / 2), (sd/se)^2 - 1) * se, 
                    ymax = mean + qt(1 - (0.05 / 2), (sd/se)^2 - 1) * se),
                width = 0.1) +
  scale_y_log10(sec.axis = sec_axis(~ . *250*7500*1.4e-7/120,
                                    name = "turnover ratio (-)",
                                    labels = trans_format("log10", math_format(10^.x)),
                                    breaks = c(10^-4, 10^-3, 10^-2, 10^-1,
                                               10^0, 10^1)),
                labels = trans_format("log10", math_format(10^.x)),
                breaks = c(10^-1, 10^0, 10^1, 10^2,
                           10^3),
                limits = c(10^-2, 10^4),
                expand = c(0, 0)) +
  geom_text(data = cor_bp_values,
            aes(x = period,
                y = mean * 4,
                label = paste0("list(",
                               signif(round(mean, 1), 2),
                               "%+-%",
                               signif(round(sd, 1), 2),
                               ")")),
            parse = TRUE,
            size = 3) +
  annotate(geom = "segment",
           x = 0,
           xend = 3,
           y = 1/(250*7500*1.4e-7/120),
           yend = 1/(250*7500*1.4e-7/120),
           color = "black",
           linetype = "dotted") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"))
p_mag_cor

# Macrophyte change
p_mag_mac <- ggplot(data = mutate_at(mac_bp_values, 2:4, function(x)x*100),
                    aes(x = as.factor(period),
                        y = mean)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = mean - qt(1 - (0.05 / 2), (sd/se)^2 - 1) * se, 
                    ymax = mean + qt(1 - (0.05 / 2), (sd/se)^2 - 1) * se),
                width = 0.1) +
  geom_blank(data = mutate(mac_bp_values, fake_value = 12.5), 
             aes(y=fake_value)) +
  geom_text(data = mutate_at(mac_bp_values, 2:4, function(x)x*100),
            aes(x = period,
                y = mean * 1.8,
                label = paste0("list(",
                               signif(mean, 1),
                               "%+-%",
                               signif(sd, 1),
                               ")")),
            parse = TRUE,
            size = 3) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"))
p_mag_mac
# Final plot and save -----------------------------------------------------
# Save all data
# save.image("Figures/Middle_Loire/Figure1_R_environment_data_final.RData")
# Skip all analyses and load Rdata

((p_solutes|p_mag_sol) +plot_layout(widths = c(6,1))) / 
   ((p_c|p_mag_cor) +plot_layout(widths = c(6,1))) /
   ((p_mac|p_mag_mac) +plot_layout(widths = c(6,1))) /
   ((p_met|p_mag_met) + plot_layout(widths = c(6,1))) +
   plot_layout(heights = c(4, 1, 1, 2)) +
    plot_annotation(tag_levels = 'a')
ggsave(filename = "Figures/Figure1_final_CI_color_final3.svg",
         device = "svg",
         dpi = 300,
         height = 250,
         width = 183,
         units = "mm")
