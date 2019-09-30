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
file_name <- "Data/RNB_stations_location/stations_RNB_Franceentiere.shp"
loc <- st_read(file_name)
plot(loc)
# Read shapefile for France rivers
file_name <- "//LY-LHQ-SRV/couches_SIG/Milieux/Hydrographie/Lineaires/carthagev4_L93/cours_eau_polyline.shp"
fr_riv <- st_read(file_name)

# Read shapefile for France departments
file_name <- "//LY-LHQ-SRV/couches_SIG/Administratives/GEOFLA/Departements_avec_dom_L93.shp"
fr_dep <- st_read(file_name)

# Only want data in Loire
l_riv <- fr_dep %>%
  filter(CODE_DEPT == 42) %>%
  st_intersection(fr_riv)

# Quick look
plot(l_riv)

tmap_mode("view")
tm_shape("World") + 
  
