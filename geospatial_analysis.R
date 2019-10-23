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
library(tmap)

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
# reaches <- st_read("Data/GIS/syrah_reaches.shp")
# reaches <- st_set_crs(reaches, 2154)

# Read in tnet shapefile
tnet <- st_read("Data/GIS/tnet_headwaters.shp")
tnet <- st_set_crs(tnet, 2154)

# Read in metadata for the reaches of concern, and filter tnet data
tnet_reaches <- read_excel("Data/Headwaters_DO/headwaters_tnet_reaches.xlsx") %>%
  left_join(tnet, by = "ID_ND_I") %>%
  # distinct(c(ID_ND_I, FID), .keep_all = TRUE) %>%
  st_as_sf() %>%
  group_by(site) %>%
  summarize()

# Load land use data
lu <- st_read("Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)

# Load vegetation data
veg <- st_read("Data/GIS/ZONE_VEGETATION_42.shp")
veg <- st_set_crs(veg, 2154)

# Only get reaches for our data points instead of the entire network
# I went into ArcGIS and specifically labeled them with "_"
# reaches <- filter(reaches, str_detect(Toponyme, "_"))

# Calculate 1, 2, 5, 10, 20, 50, and 100 m buffers around each reach
buff_dists <- list(buff_dists = c(1, 2 ,5, 10, 20, 50, 100))

# Get data in good format with sf as list column for each buffer distance
# Then buffer and intersect, calculate areas of intersection
ints <- as_tibble(buff_dists) %>%
  mutate(data = list(tnet_reaches),
         lu = list(lu),
         veg = list(veg)) %>%
  mutate(buffs = map2(data, buff_dists, st_buffer)) %>%
  mutate(ints_lu = map2(buffs, lu, st_intersection),
         ints_veg = map2(buffs, veg, st_intersection)) %>%
  mutate(area_lu = map(ints_lu, st_area),
         area_veg = map(ints_veg, st_area))

# Summarize intersection data
int_lu_sum <- ints %>%
  unnest_legacy(ints_lu, area_lu, .preserve = buff_dists) %>%
  group_by(site, buff_dists, reGROUP) %>%
  summarize(area_lu_m2 = sum(area_lu)) %>%
  mutate(area_total_m2 = sum(area_lu_m2),
         area_lu_frac = area_lu_m2 / area_total_m2)

int_veg_sum <- ints %>%
  unnest_legacy(ints_veg, area_veg, .preserve = buff_dists) %>%
  group_by(site, buff_dists, NATURE) %>%
  summarize(area_veg_m2 = sum(area_veg)) %>%
  mutate(area_total_m2 = sum(area_veg_m2),
         area_veg_frac = area_veg_m2 / area_total_m2)

# Write to disc
saveRDS(int_lu_sum, "Data/Headwaters_DO/buffer_land_use")
saveRDS(int_veg_sum, "Data/Headwaters_DO/buffer_vegetation")
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
df_lu <- int_lu_sum %>%
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
  dplyr::select(-river, -loc, -reGROUP, -area_lu_m2, 
         -area_total_m2, -landuse, -buff_dists) %>%
  spread(lu_buff, area_lu_frac)

x <- x %>%
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
  dplyr::select(-river, -loc, -reGROUP, -area_lu_m2, 
                -area_total_m2, -landuse, -buff_dists) %>%
  spread(lu_buff, area_lu_frac)



df_veg <- int_veg_sum %>%
  separate(Toponyme, c("river", "loc"),  sep = "_") %>%
  mutate(site = str_c(str_to_title(word(river, 3)
                                   ),
                      loc, 
                      sep = " "),
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
  mutate(veg_buff = str_c(NATURE, "_", buff_dists, "m")) %>%
  ungroup() %>%
  dplyr::select(-river, -loc, -area_veg_m2, 
                -area_total_m2, -NATURE, -buff_dists) %>%
  spread(veg_buff, area_veg_frac)
    
# Write to disc for regression
saveRDS(df_lu, "Data/Headwaters_DO/buffer_land_use_regression")
write_csv(df_lu, "Data/Headwaters_DO/buffer_land_use_regression.csv")

# Combine veg and land use
df <- left_join(df_lu, df_veg)

# Save all
saveRDS(df, "Data/Headwaters_DO/buffer_land_use_veg_regression")
write_excel_csv2(df, "Data/Headwaters_DO/buffer_land_use_veg_regression.csv")


# Plotting ----------------------------------------------------------------

# Plot maps
buff_data <- pluck(ints, 6, 7) %>%
  left_join(meta)
veg_data <- pluck(ints, 7, 7)

# landuse plot
lu_p <- tm_shape(buff_data) +
  tm_fill("landuse") +
  tm_facets(by = "site") +
  tm_shape(tnet_reaches) +
  tm_lines(col = "blue") +
  tm_facets(by = "site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_layout(legend.position = c(-0.3, 0.05),
            legend.width = 100)

tmap_save(lu_p, filename = "Figures/land_use_buffers.png",
          width = 12, height = 8)

veg_p <- 
  tm_shape(tnet_reaches) +
  tm_lines(col = "blue") +
  tm_facets(by = "site") +
  tm_shape(y) +
  tm_fill("NATURE", palette = "viridis") +
  tm_facets(by = "site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_layout(legend.position = c(-0.3, 0.0),
            legend.width = 100)

tmap_save(veg_p, filename = "Figures/veg_buffers.png",
          width = 12, height = 8)

# Maybe try ArcGIS binding analysis here ----------------------------------
riv <- st_read("Data/GIS/tnet_headwaters.shp")
riv <- st_set_crs(riv, 2154)

# Get rid of duplicates
riv <- distinct(riv, ID_ND_I, .keep_all = TRUE)

# Bind all rivers of the same name
riv_bind <- riv %>%
  mutate(TOPONYME1 = str_remove_all(TOPONYME1, "riviere"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivisre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivitre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivivre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "riviire"),
         TOPONYME1 = str_replace_all(TOPONYME1, "charpassone", "charpassonne"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizizy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizuzy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "viznzy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "viz zy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizezy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "ruisseau la loise", "la loise"),
         TOPONYME1 = ifelse(is.na(TOPONYME1), FID_tnet_o, TOPONYME1),
         TOPONYME1 = str_trim(TOPONYME1)) %>%
  group_by(TOPONYME1) %>%
  summarize()

# Snap points to lines
sites <- st_read("Data/GIS/site_locations.shp")
sites <- st_set_crs(sites, 2154)
site_snap <- st_snap(sites, riv_bind, tol=100)
x <- st_intersection(riv, site_snap)
plot(x)
reach_sites <- st_snap(sites_snap, riv, tol=1e-9)
reach_sites <- st_intersection(sites, riv)

# Read profile elevation data
prof <- st_read("Data/GIS/TRONCON_COURS_EAU.shp", 
                fid_column_name = "FID")
prof <- st_set_crs(prof, 2154)
?st_segmentize()






parts <- st_collection_extract(lwgeom::st_split(riv_bind$geometry, 
                                                site_snap$geometry),
                               "LINESTRING")

tm_shape(filter(riv_bind, TOPONYME1 == "la coise")) + tm_lines()
tm_shape(parts) + tm_lines() +
  tm_shape(site_snap) +tm_dots(size = 0.5)
# 

# Write to disc
st_write(df, "Data/GIS/tnet_out.shp")



# Try the landscapemetrics package ----------------------------------------
library(landscapemetrics)
library(landscapetools)
library(fasterize)
# Get data into raster format and calculate all landscape metrics by land use
lu_n2 <- ints %>%
  filter(buff_dists == 100) %>%
  unnest(c(ints_lu)) %>%
  group_by(name_site) %>%
  nest() %>%
  mutate(r = map(data, ~raster(., resolution = 100))),
         rf = map2(data, r, ~fasterize(.x, .y, field = "reGROUP"))) %>%
  mutate(metrics_l = map(rf, ~calculate_lsm(., level = "class")))

# Get data in dataframe format
lu_metrics <- lu_n %>%
  select(name_site, metrics_l) %>%
  unnest()

# Do the same for vegetation patches
veg_n <- veg %>%
  group_by(name_site) %>%
  nest() %>%
  mutate(r = map(data, ~raster(., res = 100)),
         rf = map2(data, r, ~fasterize(.x, .y, field = "reGROUP"))) %>%
  mutate(metrics_l = map(rf, ~calculate_lsm(., level = "class")))

# Get data in dataframe format
lu_metrics <- lu_n %>%
  select(name_site, metrics_l) %>%
  unnest()
# 
lu_sub <- filter(lu, name_site == "Doise")
r <- raster(lu_sub, res = 100)
r <- fasterize(lu_sub, r, field = "reGROUP")
plot(pluck(lu_n,3,1))
show_landscape(r)

lsm_l_division(r)
# Calculate all metrics at the patch scale
metrics <- calculate_lsm(r, what = "patch")
metrics_l <- calculate_lsm(r, level = "class")

# Look at the correlation in the metrics
show_correlation(metrics_l, method = "pearson")

lsm_l_ent(r)
