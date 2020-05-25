# 
# Purpose: To plot first round of DO data
# Author: Jake Diamond
# Date: July 17, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("//ly-lhq-srv/jake.diamond/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(ggmap)
library(sf)
library(scales)
# library(ggpubr)

# # #Set your API Key
# 
# 
# # Get main satellite map
# plot_loc <- get_map(location = c(lon = 4.223, 
#                                  lat = 45.637), 
#                     zoom = 11, 
#                     maptype = "satellite")
# 
# # Get river data
# riv <- st_read("Data/GIS/loire_headwater_rivers.shp")
# riv <- st_transform(riv, crs = 102110)
# riv <- st_transform(riv, crs = 3857)
# plot(riv)
# st_crs(riv)
# library(rgdal)
# riv <- readOGR("Data/GIS", "loire_headwater_rivers")
# riv@data$id <- rownames(riv@data)
# r <- fortify(riv)
# r <- left_join(r, riv@data, by = "id")
# 
# # Get watershed data
# ws <- st_read("Data/GIS/loire_headwater_subwatersheds.shp")
# 
# ws <- st_transform(ws, crs = 2154)
# ws <- st_transform(ws, crs = 3857)
# ggmap_bbox <- function(map) {
#   if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
#   # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
#   # and set the names to what sf::st_bbox expects:
#   map_bbox <- setNames(unlist(attr(map, "bb")), 
#                        c("ymin", "xmin", "ymax", "xmax"))
#   
#   # Coonvert the bbox to an sf polygon, transform it to 3857, 
#   # and convert back to a bbox (convoluted, but it works)
#   bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
#   
#   # Overwrite the bbox of the ggmap object with the transformed coordinates 
#   attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
#   attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
#   attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
#   attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
#   map
# }
# 
# 
# test_map <- ggmap_bbox(plot_loc)
# # Create a map with all of the crime locations plotted.
# ggmap(plot_loc) +
#   # geom_sf(data = ws, alpha = 0.2, inherit.aes = FALSE) +
#   # geom_sf(data = riv, color = "blue", size = 1, inherit.aes = FALSE) +
#   # geom_path(data = r,
#   #           aes(x = long, y = lat,
#   #               group = group),
#   #           colour = "blue",
#   #           size = 1) +
#   geom_point(data = meta,
#              aes(x = Longitude,
#                  y = Latitude,
#                  colour = Subwatershed),
#              size = 3) + 
  
# 
#   
# ggmap(plot_loc) + 
#   # geom_sf(data = riv,
#   #         aes(color = Rang),
#   #         inherit.aes = FALSE) + 
#   coord_sf(crs = 3857) +
#   theme_minimal()


# Set ggplot theme
th <- theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    panel.grid = element_blank()
  )

# Load data
# First get data path and names of files
data_path <- "Data/Headwaters_DO/HOBO_raw_csv"
files <- dir(data_path, pattern = "*.csv")

# Then load all data into one dataframe
df <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~ read_csv(file.path(data_path, .),
                                        skip = 2,
                                        col_names = FALSE))
  ) %>%
  unnest(cols = c(file_contents))

# Load metadata
meta <- read_excel("Data/Headwaters_DO/sensor_metadata.xlsx",
                   sheet = 2,
                   col_types = c("numeric", "text", "text", 
                                 "text", "text",
                                 "numeric", "numeric",
                                 "text", "numeric",
                                 "text", "numeric", "text", "text")) %>%
  select(-4) %>%
  rename(sensor = `DO Serial Number`)

# Some data cleaning, make filename = sensor serial number, and correct datetime
df <- df %>%
  separate(filename, c("sensor", "recoverydate"), "_") %>%
  select(-X1, -recoverydate) %>%
  mutate(X2 = mdy_hms(X2),
         Year = year(X2)) %>%
  rename(datetime = X2,
         temp = X4,
         DO = X3) %>%
  left_join(meta)

# Load point measurements
pts <- read_excel("Data/Headwaters_DO/Field_data.xlsx") %>%
  left_join(meta) %>%
  rename(temp = `T (°C)`, DO = `DO (mg/L)`) %>%
  filter(Datetime > ymd("2020-01-01"))

# plot DO data
# define limits to axes
ylim.prim <- c(-0.5, 12)   
ylim.sec <- c(10, 30)

# Calculate the plot variables for the axes
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

# Get x-axis breaks
xbrks <- pretty_dates(df$datetime, 10)

# Get 1% point of x length for label
dt_uni <- unique(df$datetime)
xpos <- dt_uni[order(as.POSIXct(dt_uni))][floor(0.01 * length(dt_uni))]

# Plot
p <- ggplot() +
  geom_line(data = df, aes(x = datetime,
                  y = DO),
            color = "black") +
  geom_line(data = df, aes(x = datetime,
                y = a + temp * b),
            color = "red") +
  geom_point(data = pts, aes(x = Datetime,
                      y = DO),
             color = "black",
             size = 2) + 
  geom_point(data = pts, aes(x = Datetime,
                             y = a + temp * b),
             color = "red",
             size = 2) + 
  scale_x_datetime(breaks = xbrks,
                   date_labels = "%d") +
  scale_y_continuous(limits = c(-1, 12),
                     breaks = seq(0, 12, 3),
                     sec.axis = sec_axis(~ (. - a) / b, 
                                         name = expression("Stream temperature "
                                                           *(degree*C))
                                         )
                     ) +
  facet_grid(rows = vars(Subwatershed_order),
             cols = vars(Subwatershed)) + 
  geom_text(data = df, aes(x = ymd_hms(xpos),
                y = -0.65,
                label = Location,
                hjust = "left"),
            size = 4) +
  # facet_wrap(~Site,
  #            labeller = label_wrap_gen(width = 18)) + 
  ylab(expression("DO (mg "*L^-1*")")) +
  th +
  theme(
    # panel.grid.major.x = element_line(colour = "light grey", 
    #                                           size = 1.4),
        # strip.text.x = element_text(size = 6,
        #                             margin = margin(0,0,0,0, "cm")),
        # axis.text.x = element_text(size = 6),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"))
  
p

# Save
ggsave(filename = "Figures/initial_DO_timeseries_ordered_v3.png",
       device = "png",
       dpi = 300,
       width = 16,
       height = 8,
       units = "in")
