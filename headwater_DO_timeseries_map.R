# 
# Purpose: To headwater DO data on a map
# Author: Jake Diamond
# Date: September 24, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(ggmap)
library(scales)
library(grid)
library(sf)

# Set your API Key
register_google(key = "AIzaSyCUFlGlYPLtIqC99Fv_xy_XabflfVG9XXM")

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
  unnest()

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
df <- df %>%
  separate(filename, c("sensor", "recoverydate"), "_") %>%
  select(-X1, -recoverydate) %>%
  mutate(X2 = mdy_hms(X2)) %>%
  rename(datetime = X2,
         temp = X4,
         DO = X3) %>%
  left_join(meta)

# Plotting function
my_plot_fun <- function(data){
  # define limits to axes
  ylim.prim <- c(-0.5, 12)   
  ylim.sec <- c(10, 30)
  
  # Calculate the plot variables for the axes
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- b*(ylim.prim[1] - ylim.sec[1])
  
  # Get x-axis breaks
  xbrks <- pretty_dates(data$datetime, 10)
  
  # Get 1% point of x length for label
  dt_uni <- unique(data$datetime)
  xpos <- dt_uni[order(as.POSIXct(dt_uni))][floor(0.01 * length(dt_uni))]
  
  ggplot() +
    geom_line(data = data, aes(x = datetime,
                             y = DO),
              color = "black") +
    geom_line(data = data, aes(x = datetime,
                             y = a + temp * b),
              color = "red") +
    scale_x_datetime(breaks = xbrks,
                     date_labels = "%d") +
    scale_y_continuous(limits = c(-1, 12),
                       breaks = seq(0, 12, 3),
                       sec.axis = sec_axis(~ (. - a) / b, 
                                           name = expression("Stream temperature "
                                                             *(degree*C))
                       )
    ) +
    # geom_text(data = data, aes(x = ymd_hms(xpos),
    #                          y = -0.65,
    #                          label = Location,
    #                          hjust = "left"),
    #           size = 4) +
    ylab(expression("DO (mg "*L^-1*")")) +
    theme_void() #+
    # theme(
    #   strip.background.y = element_blank(),
    #   strip.text.y = element_blank(),
    #   axis.line.y.right = element_line(color = "red"), 
    #   axis.ticks.y.right = element_line(color = "red"),
    #   axis.text.y.right = element_text(color = "red"), 
    #   axis.title.y.right = element_text(color = "red"))
}

findboxes <- function(
  df, xcol, ycol,
  box_padding_x, box_padding_y,
  point_padding_x, point_padding_y,
  xlim, ylim,
  force = 1e-7, maxiter = 20000
) {
  
  # x and y posiitons as a dataframe
  posdf <- df[c(xcol, ycol)]
  
  # returnd a df where columns are points
  boxdf <- apply(posdf, 1, function(row) {
    xval <- row[xcol]
    yval <- row[ycol]
    return(c(
      xval - box_padding_x / 2,
      yval - box_padding_y / 2,
      xval + box_padding_x / 2,
      yval + box_padding_y / 2
    ))
  })
  # columns are x1,y1,x2,y2
  boxmatrix <- as.matrix(t(boxdf))
  
  moved <- ggrepel:::repel_boxes(
    data_points = as.matrix(posdf),
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = boxmatrix,
    xlim = xlim,
    ylim = ylim,
    hjust = 0.5,
    vjust = 0.5,
    force = force,
    maxiter = maxiter
  )
  
  finaldf <- cbind(posdf, moved)
  names(finaldf) <- c("Longitude", "Latitude", "Longitude2", "Latitude2")
  return(finaldf)
}

# Nest data by lat long
df_n <- df %>%
  nest(-Longitude,-Latitude)

# Add new lat and long for repelled box
df_n <- findboxes(df_n, xcol = 'Longitude', ycol='Latitude',
                   box_padding_x = Reduce("-", rev(range(df_n$Longitude))) * 0.02,
                   box_padding_y = Reduce("-", rev(range(df_n$Latitude))) * 0.04,
                   point_padding_x = Reduce("-", rev(range(df_n$Longitude))) * 0.02,
                   point_padding_y = Reduce("-", rev(range(df_n$Latitude))) * 0.04,
                   force = 0.02,
                   xlim = c(min(df_n$Longitude), max(df_n$Longitude)),
                   ylim = c(min(df_n$Latitude), max(df_n$Latitude))) %>%
  right_join(df_n)
x <-select(df_n,-data)
# Annotation function
annotation_fun <- function(data, Latitude2, Longitude2, plot_fun) {
  subplot <- plot_fun(data)
  sub_grob <- inset(ggplotGrob(subplot), 
                                xmin = Longitude2-0.02, ymin = Latitude2-0.02, 
                                xmax = Longitude2+0.02, ymax = Latitude2+0.02)
}

# Get all insets
subgrobs <- df_n %>% 
  unnest() %>%
  nest(-Latitude2, -Longitude2)  %>%
  pmap(annotation_fun, plot_fun = my_plot_fun)

# Plot data
p + 
  subgrobs +
  geom_segment(data = df_n, aes(x = Longitude, y = Latitude, 
                   xend = Longitude2, yend = Latitude2)) +
  geom_point(data = df_n, aes(x = Longitude, y = Latitude), color = "black")


# # Get river data
riv <- st_read("Data/GIS/loire_headwater_rivers.shp")
riv <- st_set_crs(riv, 2154)
riv <- st_transform(riv, 
                   "+proj=longlat +init=epsg:3857")
riv <- st_transform(riv, crs = 3857)
plot(riv)
st_crs(riv)
library(rgdal)
riv <- readOGR("Data/GIS", "loire_headwater_rivers")
riv@data$id <- rownames(riv@data)
r <- fortify(riv)
r <- left_join(r, riv@data, by = "id")
#
# # Get watershed data
ws <- st_read("Data/GIS/loire_headwater_delineated_subwatersheds.shp")
ws <- st_set_crs(ws, 2154)
st_crs(ws)
ws <- st_transform(ws, 
                   "+proj=longlat +init=epsg:4326")
ws <- st_transform(ws, crs = 3857)
st_crs(ws)
proj4string()

# # Create a map with all of the crime locations plotted.
p +
  geom_sf(data = ws, alpha = 0.4, color = "red", inherit.aes = FALSE) +
  # coord_sf(crs = 3857)
  geom_sf(data = riv, color = "blue", size = 1, inherit.aes = FALSE) +
  # geom_path(data = r,
  #           aes(x = long, y = lat,
  #               group = group),
  #           colour = "blue",
  #           size = 1) +
  geom_point(data = meta,
             aes(x = Longitude,
                 y = Latitude,
                 colour = Subwatershed),
             size = 3) +

#
#
ggmap(plot_loc) +
  # geom_sf(data = riv,
  #         aes(color = Rang),
  #         inherit.aes = FALSE) +
  coord_sf(crs = 3857) +
  theme_minimal()

  
  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric  vector, 
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")), 
                         c("ymin", "xmin", "ymax", "xmax"))
    
    # Coonvert the bbox to an sf polygon, transform it to 3857, 
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates 
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }
  
  # Use the function:
  mapModified <- ggmap_bbox(plot_loc)
  
  ggmap(mapModified) + 
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = ws, alpha = 0.4, color = "red", inherit.aes = FALSE)
  