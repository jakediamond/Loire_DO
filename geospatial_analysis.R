library(sf)

# reaches <- st_read("Data/GIS/jake/Export_Output.shp")
# tnet <- read.csv("Data/GIS/all_B_H_1112_0613.csv")
# 
# df <- left_join(reaches, tnet) 
# st_write(df, "Data/GIS/tnet_out.shp")
setwd("C:/Users/jake.diamond/Dropbox/Projects/Loire_DO")
df_k <- read_xlsx("Data/Headwaters_DO/hydraulic_data.xlsx")

# Estimate K600 with 4 diff equations from Raymond 2012
# Then average, calculate Schmidt number assuming T = 22 deg C
# Then calculate K_O2, and K_2, and the estimated reach length
df_k2 <- df_k %>%
  mutate(k600_eq1 = (velocity_mps * slope_tnet)^0.89 * depth_m^0.54 * 5037,
         k600_eq3 = 1162 * slope_tnet^0.77 * velocity_mps^0.85,
         k600_eq4 = (velocity_mps * slope_tnet)^0.76 * 951.5,
         k600_eq5 = velocity_mps * slope_tnet * 2841 + 2.02) %>%
  mutate(k600_avg = rowMeans(dplyr::select(., starts_with("k600"))),
         Sc = 1801 - 120.1 * 22 + 3.782 * 22^2 -0.0476 * 22^3,
         k_o2 = k600_avg * sqrt(600/Sc),
         k_2 = k_o2 / depth_m,
         reach_length = 3 * velocity_mps * 86400 / k_2)
st_write()

# Read in reach data for each point
# These reach lengths are calculated based on estimated k2 and velocity from
# the equation 3v/k2
reaches <- st_read("Data/GIS/syrah_reaches.shp")
reaches <- st_set_crs(reaches, 2154)

# Load land use data
lu <- st_read("Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)

# Only get reaches for our data points instead of the entire network
# I went into ArcGIS and specifically labeled them with "_"
reaches <- filter(reaches, str_detect(Toponyme, "_"))

# Calculate 1, 2, 5, 10, 20, 50, and 100 m buffers around each reach
buff_dists <- c(1, 2 ,5, 10, 20, 50, 100)
buffs <- reaches %>%
  # mutate(buff_dists = buff_dists)
  mutate(buff1 = st_buffer(reaches, 1)$geometry,
         buff2 = st_buffer(reaches, 2)$geometry,
         buff5 = st_buffer(reaches, 5)$geometry,
         buff10 = st_buffer(reaches, 10)$geometry,
         buff20 = st_buffer(reaches, 20)$geometry,
         buff50 = st_buffer(reaches, 50)$geometry,
         buff100 = st_buffer(reaches, 100)$geometry) %>%
  # st_drop_geometry() %>%
  gather(key = "buff_width",
         value = "buff_geom",
         buff1:buff100) %>%
  nest(geometry:buff_geom)

         
int <- buffs %>%
  mutate(int = map(.$data, st_intersection, lu))


x <- map(reaches$buff_geom, st_intersection, lu)
int_sub <- int %>%
  filter(str_detect(Toponyme, "_")) %>%
  group_by(Toponyme, reGROUP) %>%
  summarize(area_m2 = sum(Shape_Area)) %>%
  mutate(area_total_m2 = sum(area_m2),
         area_frac = area_m2 / area_total_m2)


unique(int_sub$Toponyme)
contains()
ggplot(riv) +geom_sf()
ggplot(lu) +geom_sf(aes(fill = as.factor(code_clc)))
riv_buffer <- st_buffer(riv, 10)

ggplot(riv_buffer) +geom_sf()
buffer_lu <- st_intersection(riv_buffer, lu)
head(buffer_lu)

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
parts
ggplot() +geom_sf(data = site_snap) + geom_sf(data = riv, aes(color = as.factor(ID_TRONCON)),
                                              show.legend = FALSE)

ggplot() +geom_sf(data = site_snap) + geom_sf(data = riv_bind, aes(color = CGENELIN),
                                              show.legend = FALSE) +
  scale_color_viridis_d()
