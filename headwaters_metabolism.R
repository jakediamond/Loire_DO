# 
# Purpose: To estimate metabolism at Loire headwaters
# Author: Jake Diamond
# Date: October 16, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

# DO data load and clean --------------------------------------------------
df <- readRDS("Data/Headwaters_DO/DO_time_series") %>%
  select(datetime, DO.obs = DO, temp.water = temp,
         Site, Location, Subwatershed, Watershed, Latitude, Longitude)

# Force the correct time zone
df$datetime <- force_tz(df$datetime, "Etc/GMT+1")

# Prepare data for stream Metabolizer -------------------------------------
# Florentina's DO.sat calculation
df$DO.sat <- ifelse(df$temp.water == 0,
                    0,
                    14.652 - 0.41022 * df$temp.water + 0.007991 * 
                      df$temp.water^2 - 0.000077774 * df$temp.water^3)
 
# Convert to solar time at Gien station
df$solar.time <- calc_solar_time(df$datetime, longitude = 2.5)

# Get rid of datetime
df$datetime <- NULL

# Caclculate light
df$light <- calc_light(solar.time = df$solar.time,
                       latitude = df$Latitude,
                       longitude = df$Longitude)

# Calculate depth
df$depth <- 0.2

# Nest data by site
df_n <- df %>%
  select(-Latitude, -Longitude) %>%
  group_by(Site, Location, Subwatershed, Watershed) %>%
  distinct() %>%
  arrange(solar.time) %>%
  nest() 

# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will not pool K600 because we don't have discharge data
bayes_mod <- mm_name(type = "mle")
bayes_mod

# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, bayes_name = bayes_mod){
  # Set the specifications
  bayes_specs <- specs(model_name = bayes_name)
  
  # Do the metabolism
  metab(specs = bayes_specs, 
        data = as.data.frame(data))
}

# lad <- metab(specs = specs(model_name = "mle"), 
#              data = as.data.frame(filter(df, Site == "Loise amont Doise Salt") %>%
#                                     select(-Site, -Location, -Subwatershed,
#                                            -Watershed, -Latitude, -Longitude) %>%
#                                     arrange(solar.time) %>%
#                                     distinct()))
# ladm <- predict_metab(lad)
# Run the metabolism model on nested data ---------------------------------
mm_all_mle <- df_n %>%
  # filter(Site %in% c(#"Loise Essertine en Donzy",
  #                    
  #                    # "Doise",
  #                    "Loise aval Doise Salt")) %>%
  mutate(mm = map(data, ~met_fun(data = .x)))

saveRDS(mm_all_mle, "Data/Headwaters_DO/headwaters_metabolism_mle")
# Inspect the model -------------------------------------------------------
mm_mle <- mm_all_mle %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met)
saveRDS(mm_mle, "Data/Headwaters_DO/headwaters_metabolism_mle_preds")
mm_all_mle <- readRDS("Data/Headwaters_DO/headwaters_metabolism_mle_preds")

tp <- mm_all_mle %>%
  filter(Site == "Toranche Pontet") %>%
  select(mm) %>%
  pluck(1, 1)
plot_metab_preds(tp)
plot_DO_preds(tp)
plot(get_params(tp)$K600.daily, get_params(tp)$ER.daily)

test2 <- mm_all_mle %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  # filter(Site == "Loise Essertine en Donzy",
  #        !(between(date, 
  #                  ymd("2019-07-13"),
  #                  ymd("2019-07-20"))),
  #        !(between(date, 
  #                  ymd("2019-08-02"),
  #                  ymd("2019-08-06"))),
  #        !(between(date, 
  #                  ymd("2019-09-15"),
  #                  ymd("2019-09-24")))
  # ) %>%
  gather(key = flux,
         value = value,
         c(GPP, ER))

flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")

mm_all_mle <- mm_all_mle %>%
  filter(!(Site %in% flood_sites1 & between(date, 
                                            ymd("2019-08-06"),
                                            ymd("2019-08-29"))),
         !(Site %in% flood_sites2 & between(date, 
                                            ymd("2019-08-06"),
                                            ymd("2019-08-30"))),
         !(Site == "Coise aval Montrond" & between(date, 
                                                   ymd("2019-08-06"),
                                                   ymd("2019-09-11"))),
         !(Site == "Loise amont Doise Salt" & between(date, 
                                                      ymd("2019-07-16"),
                                                      ymd("2019-07-20"))),
         !(Site == "Loise Essertine en Donzy" & between(date, 
                                                        ymd("2019-07-20"),
                                                        ymd("2019-07-23"))),
         !(Site == "Vizézy amont Bullieux" & between(date, 
                                                     ymd("2019-07-08"),
                                                     ymd("2019-07-16"))),
         !(Site == "Vizézy amont Bullieux" & between(date, 
                                                     ymd("2019-07-24"),
                                                     ymd("2019-08-29"))),
         !(Site == "Toranche Pontet" & between(date, 
                                               ymd("2019-07-12"),
                                               ymd("2019-07-21"))),
         !(Site == "Toranche Pontet" & between(date, 
                                               ymd("2019-07-23"),
                                               ymd("2019-07-29"))),
         !(Site == "Toranche Pontet" & between(date, 
                                               ymd("2019-07-31"),
                                               ymd("2019-08-06"))),
         !(Site == "Doise" & between(date, 
                                     ymd("2019-07-14"),
                                     ymd("2019-07-20"))),
         !(Site == "Doise" & between(date, 
                                     ymd("2019-08-03"),
                                     ymd("2019-08-06"))),
         between(GPP, 0, 10),
         between(ER, -25, 0)
  )

mm_l <- gather(mm_all_mle,
               flux,
               value,
               c(GPP, ER))

ggplot(data = filter(mm_l,
                     Site == "Loise amont Doise Salt",
                     flux %in% c("ER")),
       aes(x = date,
           y = value,
           color = flux)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(data= data = filter(doise, !(flux %in% c("ER", "GPP"))),
  #               ) +
  facet_wrap(~flux, scales = "free_y", ncol = 1) +
  annotate("rect",
           xmin = ymd("2019-07-16"), xmax = ymd("2019-07-20"),
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.9) +
  annotate("rect",
           xmin = ymd("2019-08-05"), xmax = ymd("2019-08-06"),
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.9) +
  # annotate("rect",
  #          xmin = ymd("2019-09-15"), xmax = ymd("2019-09-24"),
  #          ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.9) +
  # annotate("rect",
  #          xmin = ymd("2019-09-30"), xmax = ymd("2019-10-03"),
  #          ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.9) +
  scale_y_continuous(limits = c(-20, 0))



test <- mm_mle %>%
  filter(Site == "Doise") %>%
  left_join(mm_mle %>%
              filter(Site == "Loise aval Doise Salt"), by = "date")
ggplot(data = test,
       aes(x = GPP.x, y = GPP.y,
           color = date)) +
         geom_point()+
  scale_y_continuous(limits = c(0, 3)) +
  scale_x_continuous(limits = c(0, 3)) + 
  geom_abline(a = 1, b = 0)


ggplot(data = filter(mm_all_mle,
                     Site %in% c("Toranche Pontet", "Doise"),
                     GPP > 0)) +
  geom_point(aes(x = date,
                 y = ER,
                 shape = Site),
             color = "black") +
  scale_x_date(limits = c(ymd("2019-09-08"),
                              ymd("2019-09-30")),
                   breaks = "4 day",
                   labels = date_format("%b-%d", tz = "GMT+2")) +
  scale_y_continuous(limits = c(-5, 0))


interm <- mm_all_mle %>%
  filter(Site %in% c("Toranche Pontet", "Doise"),
         GPP > 0,
         ER < 0) %>%
  mutate(wet = ifelse((Site == "Toranche Pontet" & 
                       (between(date, 
                                ymd("2019-07-04"),
                                ymd("2019-07-07")) |
                          between(date, 
                                  ymd("2019-07-13"),
                                  ymd("2019-08-07")) |
                          between(date, 
                                  ymd("2019-09-30"),
                                  ymd("2019-10-03")) |
                          between(date, 
                                  ymd("2019-09-13"),
                                  ymd("2019-09-24")))) |
                        (Site == "Doise" & 
                           (between(date, 
                                    ymd("2019-07-13"),
                                    ymd("2019-07-21")) |
                              between(date, 
                                      ymd("2019-08-02"),
                                      ymd("2019-08-06")) |
                              between(date, 
                                      ymd("2019-09-15"),
                                      ymd("2019-09-24")))),
                     "dry",
                     "wet")) %>%
  mutate(type = ifelse((Site == "Toranche Pontet" & 
                        (between(date, 
                                 ymd("2019-07-08"),
                                 ymd("2019-07-12")) |
                           between(date, 
                                   ymd("2019-09-08"),
                                   ymd("2019-09-12")) |
                           between(date, 
                                   ymd("2019-09-25"),
                                   ymd("2019-09-29")))) |
                         (Site == "Doise" & 
                            (between(date, 
                                     ymd("2019-07-08"),
                                     ymd("2019-07-12")) |
                               between(date, 
                                       ymd("2019-07-29"),
                                       ymd("2019-08-01")) |
                               between(date, 
                                       ymd("2019-09-10"),
                                       ymd("2019-09-14")))),
                      "just_before",
                      ifelse((Site == "Toranche Pontet" & 
                               (between(date, 
                                        ymd("2019-08-08"),
                                        ymd("2019-08-12")) |
                                  between(date, 
                                          ymd("2019-10-04"),
                                          ymd("2019-10-08")))) |
                               (Site == "Doise" & 
                                  (between(date, 
                                           ymd("2019-07-22"),
                                           ymd("2019-07-25")) |
                                     between(date, 
                                             ymd("2019-08-07"),
                                             ymd("2019-08-11")) |
                                     between(date, 
                                             ymd("2019-09-25"),
                                             ymd("2019-09-29")))),
                             "just_after",
                             "between"))) %>%
  filter(wet == "wet") %>%
  select(Site, GPP, ER, wet, type) %>%
  gather(key = flux, value = value, GPP, ER) %>%
  filter(abs(value) < 10)


interm$type2 <- factor(interm$type, levels = c("just_before", "between", "just_after"))
interm$flux2 <- factor(interm$flux, levels = c("GPP", "ER"))
interm_p <- ggplot(interm,
       aes(x = type2,
           y = value,
           color = flux2,
           shape = Site)) +
  stat_summary(fun.y = mean, geom = "point", size = 4, 
               position = position_dodge(width = 0.2)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1,
               position = position_dodge(width = 0.2)) +
  scale_color_manual(breaks = c("ER", "GPP"),
                       values = c("black", "blue"),
                     guide = FALSE) +
  scale_shape_manual(values = c(1, 16)) +
  ylab(expression("mg "~O[2]~m^{-2}~d^{-1})) +
  xlab("") +
  facet_wrap(~flux2, scales = "free_y", ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

ggsave(interm_p,
       filename = "Figures/intermittency_summary.tiff",
       device = "tiff",
       dpi = 300,
       width = 10,
       height = 8,
       units = "in")


  
  
  
  
  group_by(Site, type) %>%
  summarize(mean_gpp = mean(GPP, na.rm = TRUE),
            se_gpp = 
              mean_er = mean(ER, na.rm = TRUE))
