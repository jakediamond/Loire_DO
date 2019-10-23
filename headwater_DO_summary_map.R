# 
# Purpose: To headwater DO data on a map
# Author: Jake Diamond
# Date: September 24, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(ggmap)
library(scales)
library(grid)
library(sf)
library(ggrepel)
library(tidyverse)
library(tmap)

# Load data ---------------------------------------------------------------
# Load DO summary data
df_sum <- readRDS("Data/Headwaters_DO/DO_summary")
df_met <- readRDS("Data/Headwaters_DO/headwaters_metabolism_mle_preds")
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_met<- df_met %>%
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
         between(ER, -20, 0)
         
         
  ) %>%
  group_by(Site) %>%
  summarize(mean_GPP = mean(GPP, na.rm = TRUE),
            mean_ER = mean(ER, na.rm = TRUE))

# Load DO time series data
df_ts <- readRDS("Data/Headwaters_DO/DO_time_series") %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                          temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat)

# Load metadata
meta <- read_excel("Data/Headwaters_DO/sensor_metadata.xlsx",
                   sheet = 2,
                   col_types = c("text", "text", 
                                 "text", "text",
                                 "numeric", "numeric",
                                 "text", "numeric",
                                 "text", "numeric")) %>%
  select(-3) %>%
  rename(sensor = `Serial Number`)

# Some data cleaning, make filename = sensor serial number, and correct datetime
df_sum <- left_join(df_sum, meta)
df_met <- left_join(df_met, meta)

# Convert both time series and summary to sf object
df_ts_sf <- st_as_sf(df_ts, coords = c("Longitude", "Latitude"))
df_sum_sf <- st_as_sf(df_sum, coords = c("Longitude", "Latitude"))
df_met_sf <- st_as_sf(df_met, coords = c("Longitude", "Latitude"))
  
# # Get river data
riv <- st_read("Data/GIS/tnet_out.shp")
riv <- st_set_crs(riv, 2154)
riv <- st_transform(riv, 
                    "+proj=longlat +init=epsg:4326")
# # Get watershed data
ws <- st_read("Data/GIS/loire_headwater_delineated_subwatersheds.shp")
ws <- st_set_crs(ws, 2154)
ws <- st_transform(ws, 
                   "+proj=longlat +init=epsg:4326")
# Some data cleaning
ws <- mutate(ws,
             Site = str_remove_all(name_site, " -"),
             Site = str_remove_all(Site, "Pannissières"),
             Site = str_remove_all(Site, "moulin de Salt"),
             Site = str_replace(Site, "Donzy Salt", "Donzy"),
             Site = str_replace(Site, "aval Doise", "aval Doise Salt"),
             Site = str_replace(Site, "aval Poncins", "amont Poncins"),
             Site = str_remove_all(Site, "La "),
             Site = str_replace(Site, "Aval", "aval"),
             Site = str_trim(Site)) %>%
  left_join(meta)

# Read in some discharge data
df_q <- read_xlsx("Data/Headwaters_DO/Discharge/banque_hydro.xlsx")

# Read in landuse data
lu <- st_read("Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)
lu <- st_transform(lu, 
                   "+proj=longlat +init=epsg:4326")
lu <- mutate(lu,
             Site = str_remove_all(name_site, " -"),
             Site = str_remove_all(Site, "Pannissières"),
             Site = str_remove_all(Site, "moulin de Salt"),
             Site = str_replace(Site, "Donzy Salt", "Donzy"),
             Site = str_replace(Site, "aval Doise", "aval Doise Salt"),
             Site = str_replace(Site, "aval Poncins", "amont Poncins"),
             Site = str_remove_all(Site, "La "),
             Site = str_replace(Site, "Aval", "aval"),
             Site = str_trim(Site)) %>%
  left_join(meta)
lu$reGROUP <- as.factor(lu$reGROUP)

# ggmaps for satellite imagery --------------------------------------------
# Set your API Key
register_google(key = "AIzaSyCUFlGlYPLtIqC99Fv_xy_XabflfVG9XXM")

# Nest data by mean lat and long of watershed for plotting purposes
df_n <- df_sum_sf %>%
  left_join(ws %>%
              group_by(Watershed) %>%
              filter(Shape_Area == max(Shape_Area)) %>%
              mutate(
                        bbox = map(geometry, st_bbox),
                        bbox_poly = map(bbox, st_as_sfc),
                        cent = map(bbox_poly, st_centroid),
                        long_mean = map(cent, c(1, 1)),
                        lat_mean = map(cent, c(1, 2)),
                        ) %>%
              unnest(long_mean, lat_mean) %>%
              as_tibble() %>%
              select(long_mean, lat_mean, Watershed)) %>%
  ungroup() %>%
  nest(-long_mean, -lat_mean, -Watershed)

# Plotting function
plot_fun <- function(plot_loc, watershed){
  set.seed(42)
  ws_plot = filter(ws, Watershed == watershed)
  riv_plot = st_intersection(riv, ws_plot)
  brks = tibble(OSTRAHL = c(1,2,3,4,5),
                brks = c(0.6, 0.8, 1, 1.4, 1.6))
  riv_plot = left_join(riv_plot, brks)
  bbox = st_bbox(ws_plot)
  ggmap(plot_loc) + 
    geom_sf(data = ws_plot, alpha = 0.1, 
            color = "black", size = 1.2, inherit.aes = FALSE) +
    geom_sf(data = riv_plot, aes(size = brks),
            color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
    xlab("") + ylab("") +
    ggtitle(watershed) +
    scale_size_identity() +
    geom_point(data = filter(df, Watershed == watershed),
               aes(x = Longitude,
                   y = Latitude,
                   color = min),
               size = 2.2) +
    geom_text_repel(data = filter(df, Watershed == watershed),
                     aes(x = Longitude,
                         y = Latitude,
                         label = Site),
                    color = "white") +
    scale_color_viridis_c(name = "Min. DO (mg/L)", option = "magma",
                          limits = c(3.5, 9)) +
    # scale_size_continuous(name = "Amp. quo. \n moy. (mg/L)") +
    scale_x_continuous(limits = c(bbox[1]-0.01, bbox[3]+0.01), expand = c(0, 0)) +
    scale_y_continuous(limits = c(bbox[2], bbox[4]), expand = c(0, 0))
}

# Get base map for each watershed
df_n <- df_n %>%
  group_by(Watershed) %>%
  mutate(plot_loc = map2(long_mean, lat_mean, ~get_map(location = c(.x, .y),
                                                       maptype = "satellite",
                                                       zoom = 11)
                         ),
         plots = map2(plot_loc, Watershed, ~plot_fun(.x, .y)
                      )
         )

# Save data 
walk2(df_n$Watershed,
      df_n$plots, 
        ~ggsave(plot = .y,
              filename = paste0("Figures/",
                                .x,
                                "_satellite_minDO_zoom11.tiff"),
              device = "tiff",
              dpi = 300,
              width = 18.4,
              height = 18.4,
              unit = "cm"))

# Static map of min DO with tmap ------------------------------------------------
# Animations
# Join discharge data with geometry data
riv2 <- df_q %>%
  mutate(Site = str_replace(site, "Coise Moulin Brûlé", "Coise amont St Symphorien")) %>%
  right_join(riv %>%
               mutate(Site = ifelse(str_detect(TOPONYME1, "coise"),
                                    "Coise amont St Symphorien",
                                    NA)
                      )
             ) %>%
  st_as_sf()

# Intersect river data with headwaters data
riv_hw <- st_intersection(ws, riv)

df_met_sf$PR <- df_met_sf$mean_GPP / abs(df_met_sf$mean_ER)
# Plot minimum DO with land use
p_tm <- tm_shape(ws) + tm_borders() +
  tm_facets(by = "Watershed") +
  tm_shape(riv_hw) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 1.6) +
  tm_facets(by = "Watershed") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(lu) + 
  tm_fill(title = "Land use",
          col = "reGROUP", alpha = 0.3,
          breaks = c(11, 12, 21, 22, 31, 32, 50),
          labels = c("Terr. art. dense",
                     "Terr. art. discon.",
                     "Ag. int.",
                     "Ag. fai. imp.",
                     "Forêt",
                     "Pelouses, friches",
                     "Espace ouverts",
                     "Zones humides",
                     "Surface en eaux"),
          palette = "Spectral") +
  tm_facets(by = "Watershed") +
  tm_shape(df_met_sf) +
  tm_symbols(
          col = "PR", size = 0.5,
          # breaks = c(0, 3, 4, 6, 8, 10),
          palette = "viridis",
          border.col = "black",
          border.lwd = 1.2,
          title.col = "Mean P/R") +
  tm_facets(by = "Watershed")

# Get an inset map
# Inset map of just rivers
region_map <- tm_shape(ws) + tm_polygons() +
  tm_shape(riv_hw) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 2) +
  tm_shape(filter(riv,
                  str_detect(TOPONYME1, "loire"))) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 3) +
  tm_shape(df_met_sf) +
  tm_symbols(
    col = "PR", size = 0.3,
    # breaks = c(0, 3, 4, 6, 8, 10),
    palette = "viridis",
    border.col = "black",
    border.lwd = 1.2,
    title.col = "Mean P/R",
    legend.col.show = FALSE)

# Save map
tmap_save(p_tm, filename = "Figures/land_use_PR.png",
          width = 6, height = 6,
          insets_tm = region_map,
          insets_vp = viewport(0.52, 0.18, width = 0.33, height = 0.45))
  

# Animation maps ----------------------------------------------------------
df_test <- filter(df_ts_sf,
                  Watershed == "Loise",
                  between(datetime, 
                          ymd_hms("2019-09-01 00:00:00"),
                          ymd_hms("2019-09-11 00:00:00"))) %>%
  group_by(Site, datetime = cut(datetime, breaks = "1 hours")) %>%
  summarize(DO_per = mean(DO_per, na.rm = TRUE)) %>%
  arrange(Site, datetime) %>%
  mutate(hour = hour(datetime),
         hour2 = ifelse(hour>12,
                        hour - 2 * (hour - 12),
                        hour))


anim_test <- tm_shape(filter(ws, Watershed == "Loise")) + tm_borders() + 
  tm_shape(filter(riv_hw, Watershed == "Loise")) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE) +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(df_test) + 
  tm_bubbles(col = "Watershed_order", size = "DO_per",
          palette = viridisLite::viridis(12),
          perceptual = TRUE,
          size.max = 110,
          size.lim = c(30, 100),
          sizes.legend = c(0, 40, 80, 100),
          legend.col.show = FALSE) +
  tm_facets(along = "datetime", free.coords = FALSE,
            nrow = 1, ncol = 1) +
  tmap_options(limits = c(facets.plot = 241))

tmap_animation(anim_test, filename = "Figures/anim_test.gif",
               width = 6, height = 4,
               delay = 1)
anim_test




# Animate DO time series --------------------------------------------------
library(gganimate)

# Get rid of bad data when sensors were buried or when out of water
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_ts_sub <- df_ts %>%
  filter(!(Site %in% flood_sites1 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-29 16:45"))),
         !(Site %in% flood_sites2 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-30 10:30"))),
         !(Site == "Coise aval Montrond" & between(datetime, 
                                                   ymd_hm("2019-08-06 16:00"),
                                                   ymd_hm("2019-09-11 11:30"))),
         !(Site == "Loise amont Doise Salt" & between(datetime, 
                                                      ymd_hm("2019-07-16 00:30"),
                                                      ymd_hm("2019-07-20 20:00"))),
         !(Site == "Loise aval Doise Salt" & between(datetime, 
                                                     ymd_hm("2019-08-06 19:00"),
                                                     ymd_hm("2019-08-30 10:15"))),
         !(Site == "Loise Essertine en Donzy" & between(datetime, 
                                                        ymd_hm("2019-07-20 22:15"),
                                                        ymd_hm("2019-07-23 15:30"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                     ymd_hm("2019-07-08 19:00"),
                                                     ymd_hm("2019-07-16 14:15"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                     ymd_hm("2019-07-24 23:00"),
                                                     ymd_hm("2019-08-29 15:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-12 19:45"),
                                               ymd_hm("2019-07-21 17:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-23 22:45"),
                                               ymd_hm("2019-07-29 01:00"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-31 03:45"),
                                               ymd_hm("2019-08-06 17:30"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-07-14 15:00"),
                                     ymd_hm("2019-07-20 17:15"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-08-03 20:15"),
                                     ymd_hm("2019-08-06 12:30")))
         
  )

# Only Loise watershed
df_ts_l <- filter(df_ts_sub,
                  Watershed == "Loise",
                  between(datetime, 
                          ymd_hms("2019-09-01 00:00:00"),
                          ymd_hms("2019-09-11 00:00:00"))) %>%
  group_by(Site, datetime = cut(datetime, breaks = "1 hours")) %>%
  ungroup() %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  group_by(Site, datetime) %>%
  summarize(DO_per = mean(DO_per, na.rm = TRUE)) %>%
  arrange(Site, datetime) %>%
  mutate(hour = hour(datetime),
         hour2 = ifelse(hour>12,
                        hour - 2 * (hour - 12),
                        hour)) %>%
  left_join(meta) %>%
  ungroup()

df_ts_l_sub <- filter(df_ts_l,
                       Location %in% c("la Jamarie",
                                       "Moulin Piquet",
                                       # "Château de Donzy",
                                       "Feurs",
                                       "aval Doise Salt"))

p_anim_loise <- ggplot(data = df_ts_l_sub, 
            aes(x = datetime, 
                y = DO_per,
                group = Watershed_order,
                color = as.factor(Watershed_order))) +
  geom_line(size = 2,
            alpha = 0.7,
            show.legend = FALSE)  +
  # geom_point(size = 3,
  #            show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, 50)) +
  scale_color_viridis_d(name = "Watershed order") +
  # transition_reveal(datetime) + 
  theme_bw() + 
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 125),
                     breaks = seq(0, 125, 25)) +
  xlab("") +
  ylab("DO (% sat.)") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18))
  
ggsave(plot = p_anim_loise, filename="Figures/loise_points_v2.tiff",
       dpi = 300, width = 8, height = 6, units = "in")
anim_save(animation = p_anim_loise, 
          filename="Figures/loise_points_v2.gif",
          height = 800,
          width = 1000)

p_map <- ggmap(plot_loc) + 
  geom_sf(data = filter(ws, Watershed == "Loise"), 
          alpha = 0.1, 
          color = "black", size = 1.2, inherit.aes = FALSE) +
  geom_sf(data = filter(riv_hw, Watershed == "Loise"), aes(size = brks),
          color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
  xlab("") + ylab("") +
  geom_point(data = filter(df_loise_sub, counter == i),
             aes(x = Longitude,
                 y = Latitude,
                 fill = as.factor(Watershed_order),
                 size = DO_per),
             show.legend = FALSE,
             shape = 21,
             color = "white") + 
  geom_text_repel(data = filter(df_loise_sub, counter == i),
                  aes(x = Longitude,
                      y = Latitude,
                      label = Site),
                  color = "white",
                  size = 4) +
  scale_fill_viridis_d()
ggsave(p_map, filename = "Figures/Loise_map_watershed_order.tiff",
       device = "tiff",
       dpi = 300)
# Side by side with patchwork library -------------------------------------
library(patchwork)
library(animation)
# create counter
df_loise <- df_ts_l %>%
  group_by(Site) %>%
  mutate(counter = row_number()) %>%
  ungroup()

plot_loc <- get_map(location = c(pluck(df_n, 2, 1), pluck(df_n, 3, 1)),
                    maptype = "satellite",
                    zoom = 12)

brks <- tibble(OSTRAHL = c(1,2,3,4,5),
              brks = c(0.6, 0.8, 1, 1.4, 1.6))
riv_hw <- left_join(riv_hw, brks)

df_loise_sub <- filter(df_loise,
                       Location %in% c("la Jamarie",
                                       "Moulin Piquet",
                                       # "Château de Donzy",
                                       "Feurs",
                                       "aval Doise Salt"))
# Create gif
invisible(saveGIF({
  for (i in 1:240){
    set.seed(42)
    p <- ggmap(plot_loc) + 
      geom_sf(data = filter(ws, Watershed == "Loise"), 
              alpha = 0.1, 
              color = "black", size = 1.2, inherit.aes = FALSE) +
      geom_sf(data = filter(riv_hw, Watershed == "Loise"), aes(size = brks),
              color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
      xlab("") + ylab("") +
      geom_point(data = filter(df_loise_sub, counter == i),
                 aes(x = Longitude,
                     y = Latitude,
                     fill = as.factor(Watershed_order),
                     size = DO_per),
                 show.legend = FALSE,
                 shape = 21,
                 color = "white") + 
      geom_text_repel(data = filter(df_loise_sub, counter == i),
                      aes(x = Longitude,
                          y = Latitude,
                          label = Site),
                      color = "white",
                      size = 4) +
      scale_fill_viridis_d()
    
    p2 <- ggplot() +
      geom_line(data = filter(df_loise_sub, counter <= i), 
                aes(x = datetime, 
                    y = DO_per,
                    group = Watershed_order,
                    color = as.factor(Watershed_order)),
                size = 2,
                alpha = 0.7,
                show.legend = FALSE) +
      geom_point(data = filter(df_loise_sub, counter == i), 
                 aes(x = datetime, 
                     y = DO_per,
                     group = Watershed_order,
                     color = as.factor(Watershed_order)),
                 size = 2,
                 alpha = 0.7,
                 show.legend = FALSE) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      scale_x_datetime(limits = c(min(df_loise$datetime), 
                                    max(df_loise$datetime)),
                       breaks = "24 hours",
                       labels = date_format("%H:%M", tz = "GMT-2")) +
      scale_y_continuous(limits = c(25, 110),
                         breaks = seq(25, 110, 25)) +
      scale_color_viridis_d(name = "Watershed order") + 
      theme_bw() +
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            axis.text.x = element_text(size = 8)) +
      xlab("") +
      ylab("DO (% sat.)")

    # Print plots using patchwork
    print(p + p2 + plot_layout(ncol = 2))
  }
  
  
}, movie.name = "test3.gif", interval = 0.01, ani.width = 1000, ani.height = 700))

