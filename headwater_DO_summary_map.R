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

# Set your API Key
register_google(key = "AIzaSyCUFlGlYPLtIqC99Fv_xy_XabflfVG9XXM")

# Load data
df <- readRDS("Data/Headwaters_DO/DO_summary")

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
df <- left_join(df, meta)

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

# Nest data by mean lat and long of watershed for plotting purposes
df_n <- df %>%
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

# Animations
df2 <- df2 %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                          temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat)
df_sf <- st_as_sf(df2, coords =c("Longitude", "Latitude"))
# df_sf <- st_set_crs(df_sf, 2154)
# 
# Read in some discharge data
df_q <- read_xlsx("Data/Headwaters_DO/Discharge/banque_hydro.xlsx")

df_q_w <- spread(df_q, site, discharge)
plot(df_q_w$`Lignon Poncins`, df_q_w$`Mare Saint-Marcellin-en-Forez`)

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


riv_hw <- st_intersection(ws, riv)

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
  tm_shape(st_as_sf(df, coords =c("Longitude", "Latitude"))) +
  tm_symbols(
          col = "min_DO", size = 0.5,
          breaks = c(0, 3, 4, 6, 8, 10),
          palette = c("red", "orange", "yellow", "green", "blue"),
          border.col = "black",
          border.lwd = 1.2,
          title.col = "Min. DO (mg/L)") +
  tm_facets(by = "Watershed")

riv_region <- st_combine(riv_hw,
                        filter(riv,
                               str_detect(TOPONYME1, "loire"))
                        )

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
  tm_shape(st_as_sf(df, coords =c("Longitude", "Latitude"))) + 
  tm_symbols(
    col = "min_DO", size = 0.1,
    breaks = c(0, 3, 4, 6, 8, 10),
    palette = c("red", "orange", "yellow", "green", "blue"),
    border.col = "black",
    border.lwd = 1.2,
    title.col = "Min. DO (mg/L)",
    legend.col.show = FALSE)

tmap_save(p_tm, filename = "Figures/land_use_minDO.png",
          width = 6, height = 6,
          insets_tm = region_map,
          insets_vp = viewport(0.52, 0.18, width = 0.33, height = 0.45))
  


df_test <- filter(df_sf,
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
  tm_bubbles(col = "hour2", size = "DO_per",
          palette = viridisLite::cividis(12),
          perceptual = TRUE,
          size.max = 110,
          size.lim = c(30, 100),
          sizes.legend = c(0, 40, 80, 100),
          legend.col.show = FALSE) +
  tm_facets(along = "datetime", free.coords = FALSE,
            nrow = 1, ncol = 1) +
  tmap_options(limits = c(facets.plot = 241))

tmap_animation(anim_test, filename = "Figures/anim_test.gif",
               width = 6, height = 4)
anim_test
