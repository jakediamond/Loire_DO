# 
# Purpose: To make a map for Florentina for temperature in Department 42
# Author: Jake Diamond
# Date: October 7, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(sf)
library(tmap)
library(tidyverse)

# # Get river data
riv <- st_read("Data/GIS/tnet-42_depart.shp")
riv_meta <- read_csv2("Data/GIS/all_B_H_1112_0613_CSV2.csv") %>%
  rename(OBJECTID = OBJECTID_1,
         width = B_1112_0613,
         depth = H_1112_0613,
         v = V_1112_0613)
riv <- left_join(riv, riv_meta)
# # Get watershed data
ws <- st_read("Data/GIS/loire_headwater_subwatersheds.shp")
ws <- st_set_crs(ws, 2154)
ws <- st_transform(ws, 
                   "+proj=longlat +init=epsg:4326")

# Get department
dep <- st_read("Data/GIS//Department42.shp")

# Get outlet points
df <- read_csv2("Data/outlets_points.csv", locale = locale(encoding = "latin1"))
df_sf <- st_as_sf(df, coords = c("X", "Y"))
df_sf <- st_set_crs(df_sf, 2154)
df_sf <- st_transform(df_sf, 
                   "+proj=longlat +init=epsg:4326")


p_tm <- tm_shape(dep) + tm_borders() +
  # tm_shape(ws) + tm_borders(lwd = 3) +
  tm_shape(riv) +
  tm_lines(lwd = "ReseauTN_5", col = "width",
           legend.lwd.show = FALSE,
           breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
           scale = 2,
           palette = "viridis",
           title.col = "Largeur (m)") +
  tm_shape(df_sf) +
  tm_dots(size = 0.5) +
  tm_text("nom2", size= 1, auto.placement = TRUE) +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_layout(legend.position = c("RIGHT","TOP"))
p_tm
tmap_save(p_tm, filename = "Figures/department_level_width_points.png",
          width = 6, height = 8)

p_tm2 <- tm_shape(dep) + tm_borders() +
  # tm_shape(ws) + tm_borders(lwd = 3) +
  tm_shape(riv) +
  tm_lines(lwd = "ReseauTN_5", col = "depth",
           legend.lwd.show = FALSE,
           # breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
           scale = 2,
           palette = "viridis",
           title.col = "Profondeur (m)") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_layout(legend.position = c("RIGHT","TOP"))
p_tm2
tmap_save(p_tm2, filename = "Figures/department_level_depth.png",
          width = 6, height = 8)

p_tm3 <- tm_shape(dep) + tm_borders() +
  # tm_shape(ws) + tm_borders(lwd = 3) +
  tm_shape(riv) +
  tm_lines(lwd = "ReseauTN_5", col = "v",
           legend.lwd.show = FALSE,
           # breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
           scale = 2,
           palette = "viridis",
           title.col = "Vitesse (m/s)") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_layout(legend.position = c("RIGHT","TOP"))
p_tm3
tmap_save(p_tm3, filename = "Figures/department_level_vit.png",
          width = 6, height = 8)
