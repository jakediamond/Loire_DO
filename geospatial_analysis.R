# 
# Purpose: To estimate reaeration based on GIS data, and to calculate land use
# at different scales for multiple regression analysis
# Author: Jake Diamond
# Date: October 1, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(sf)
library(tidyverse)

# Reaeration constant estimation ------------------------------------------
# Load data
# Read in hydraulic data
df_k <- read_xlsx("Data/Headwaters_DO/hydraulic_data.xlsx")

# Estimate K600 with 4 diff equations from Raymond 2012
# Then average, calculate Schmidt number assuming T = 22 deg C
# Then calculate K_O2, and K_2, and the estimated reach length
df_k <- df_k %>%
  mutate(k600_eq1 = (velocity_mps * slope_tnet)^0.89 * depth_m^0.54 * 5037,
         k600_eq3 = 1162 * slope_tnet^0.77 * velocity_mps^0.85,
         k600_eq4 = (velocity_mps * slope_tnet)^0.76 * 951.5,
         k600_eq5 = velocity_mps * slope_tnet * 2841 + 2.02) %>%
  mutate(k600_avg = rowMeans(dplyr::select(., starts_with("k600"))),
         Sc = 1801 - 120.1 * 22 + 3.782 * 22^2 -0.0476 * 22^3,
         k_o2 = k600_avg * sqrt(600/Sc),
         k_2 = k_o2 / depth_m,
         reach_length = 3 * velocity_mps * 86400 / k_2)


# Intersecting reaches with land use --------------------------------------
# Read in reach data for each point
# These reach lengths are calculated based on estimated k2 and velocity from
# the equation L = 3v/k2
reaches <- st_read("Data/GIS/syrah_reaches.shp")
reaches <- st_set_crs(reaches, 2154)

# Load land use data
lu <- st_read("Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)

# Only get reaches for our data points instead of the entire network
# I went into ArcGIS and specifically labeled them with "_"
reaches <- filter(reaches, str_detect(Toponyme, "_"))

# Calculate 1, 2, 5, 10, 20, 50, and 100 m buffers around each reach
buff_dists <- list(buff_dists = c(1, 2 ,5, 10, 20, 50, 100))

# Get data in good format with sf as list column for each buffer distance
# Then buffer and intersect, calculate areas of intersection
ints <- as_tibble(buff_dists) %>%
  mutate(data = list(reaches),
         lu = list(lu)) %>%
  mutate(buffs = map2(data, buff_dists, st_buffer)) %>%
  mutate(ints = map2(buffs, lu, st_intersection)) %>%
  mutate(area = map(ints, st_area))

# Summarize intersection data
int_sum <- ints %>%
  unnest(ints, area, .preserve = buff_dists) %>%
  group_by(Toponyme, buff_dists, reGROUP) %>%
  summarize(area_m2 = sum(area)) %>%
  mutate(area_total_m2 = sum(area_m2),
         area_frac = area_m2 / area_total_m2)

# Write to disc
saveRDS(int_sum, "Data/Headwaters_DO/buffer_land_use")
int_sum <- readRDS("Data/Headwaters_DO/buffer_land_use")

# Clean up dataframe
meta <- tibble(reGROUP = c(11, 12, 21, 22, 31, 32, 33, 40, 50),
               landuse = c("territoire artificialisé dense",
                           "territoire artificialisé discontinu",
                           "agricole intensif",
                           "agricole faible impact",
                           "forest",
                           "pelouses, landes, friches",
                           "espaces ourverts",
                           "zones humides",
                           "surfaces en eaux"))
df <- int_sum %>%
  separate(Toponyme, c("river", "loc"),  sep = "_") %>%
  mutate(site = str_c(str_to_title(word(river, 3
                                  )
                             ),
                      loc, sep = " "
                      ),
         site = str_squish(site),
         site = str_trim(site),
         site = ifelse(site == "Charpassone la Jamarie",
                       "Charpassonne la Jamarie",
                       site),
         site = ifelse(site == "Charpassone de Donzy Salt",
                       "Charpassone de Donzy",
                       site),
         site = ifelse(site == "Doise Doise",
                       "Doise",
                       site),
         site = ifelse(site == "Curraize Curraize",
                "Curraize",
                site)) %>%
  left_join(meta) %>%
  mutate(lu_buff = str_c(landuse, "_", buff_dists, "m")) %>%
  ungroup() %>%
  dplyr::select(-river, -loc, -reGROUP, -area_m2, 
         -area_total_m2, -landuse, -buff_dists) %>%
  spread(lu_buff, area_frac)
    
# Write to disc for regression
saveRDS(df, "Data/Headwaters_DO/buffer_land_use_regression")

# Quick look at how percent land cover changes over time
ggplot(int_sum) +
  geom_line(aes(x = buff_dists,
                y = area_frac,
                color = as.factor(reGROUP))) +
  facet_wrap(~Toponyme)

# Maybe a little look at the land cover too
ggplot() +
  geom_sf(data = pluck(ints, 5, 7),
          aes(fill = as.factor(reGROUP)))

# Maybe try ArcGIS binding analysis here ----------------------------------
# Bind all rivers of the same name
riv_bind <- riv %>%
  group_by(CGENELIN) %>%
  st_line_merge()


# Snap points to lines
sites <- st_read("Data/GIS/site_locations.shp")
# sites <- st_set_crs(sites, 3857)
# sites <- st_transform(sites, 2154)
site_snap <- st_snap(sites, riv, tol=1e-9)
parts <- st_collection_extract(lwgeom::st_split(riv$geometry, 
                                                site_snap$geometry),
                               "LINESTRING")

# Don't need to do this part anymore
reaches <- st_read("Data/GIS/jake/Export_Output.shp")
tnet <- read.csv("Data/GIS/all_B_H_1112_0613.csv")

# Join the tnet data (with hydraulic info) to reach spatial data
df <- left_join(reaches, tnet)

# Write to disc
st_write(df, "Data/GIS/tnet_out.shp")



# Try the landscapemetrics package ----------------------------------------
library(landscapemetrics)
library(landscapetools)
library(fasterize)
# Get data into raster format
lu_sub <- filter(lu, name_site == "Doise")
r <- raster(lu_sub, res = 1)
r <- fasterize(lu_sub, r, field = "reGROUP")
plot(r)

lsm_l_division(r)
# Calculate all metrics at the patch scale
metrics <- calculate_lsm(r, what = "patch")

# Look at the correlation in the metrics

