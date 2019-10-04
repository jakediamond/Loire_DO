# 
# Purpose: To plot long term data on a map
# Author: Jake Diamond
# Date: June 12, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(tidyverse)
library(sf)
library(tmap)

# Read in shapefile for long term sample locations
loc <- st_read("Data/GIS/loire_headwater_wq.shp")
loc <- st_set_crs(loc, 2154)
loc <- st_transform(loc, 
                   "+proj=longlat +init=epsg:4326")

# Only want data in Loire
loc_hw_riv <- st_intersection(loc, ws)

tm_shape(ws) + tm_borders() +
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
  tm_facets(by = "Watershed") +
  tm_shape(loc_hw_riv) + 
  tm_facets(by = "Watershed") +
  tm_symbols(shape = 24,
             col = "O2_min", size = 0.5,
             breaks = c(0, 3, 4, 6, 8, 10),
             palette = c("red", "orange", "yellow", "green", "blue"),
             border.col = "black",
             border.lwd = 1.2,
             title.col = "Min. DO (mg/L)") +
  tm_facets(by = "Watershed")

