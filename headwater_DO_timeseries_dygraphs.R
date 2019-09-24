# 
# Purpose: To plot first round of DO data
# Author: Jake Diamond
# Date: July 17, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(dygraphs)
library(htmltools)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggmap)
library(sf)
library(scales)
# library(ggpubr)
library(plotly)

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

# Save data
saveRDS(df, "Data/Headwaters_DO/DO_time_series")
write.csv(df, "Data/Headwaters_DO/DO_time_series.csv")

# Calculate average daily amplitude by site
df %>%
  mutate(date = date(datetime)) %>%
  dplyr::filter(DO > 1) %>%
  group_by(Subwatershed, Subwatershed_order, Site, date, Location) %>%
  summarize(mean = mean(DO, na.rm = TRUE),
            max = max(DO, na.rm = TRUE),
            min = min(DO, na.rm = TRUE),
            amp = max - min) %>%
  gather(msmt, val, amp) %>%
  ggplot(aes(x = Subwatershed_order, y = val)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Subwatershed) +
  scale_color_viridis_d() +
  theme_bw() +
  ylab('Mean daily DO amplitude')
  

# Load point measurements
pts <- read_excel("Data/Headwaters_DO/Field_data.xlsx") %>%
  left_join(meta) %>%
  rename(temp = `T (°C)`, DO = `DO (mg/L)`) %>%
  filter(Datetime > ymd("2019-07-01"))

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
                  y = DO,
                  color = as.factor(Watershed_order))) +
  # geom_line(data = df, aes(x = datetime,
  #               y = a + temp * b,
  #               color = Subwatershed_order),
  #           linetype = "dashed") +
  scale_x_datetime(breaks = xbrks,
                   date_labels = "%d") +
  scale_y_continuous(limits = c(-1, 12),
                     breaks = seq(0, 12, 3),
                     # sec.axis = sec_axis(~ (. - a) / b, 
                     #                     name = expression("Stream temperature "
                     #                                       *(degree*C))
                     #                     )
                     ) +
  facet_grid(rows = vars(Watershed)) + 
  scale_color_viridis_d(name = "Watershed order") +
  geom_text(data = df, aes(x = ymd_hms(xpos),
                y = -0.2,
                label = Watershed,
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
        strip.text.y = element_blank()
        # , axis.line.y.right = element_line(color = "red"), 
        # axis.ticks.y.right = element_line(color = "red"),
        # axis.text.y.right = element_text(color = "red"), 
        # axis.title.y.right = element_text(color = "red")
        )
  
p

# Save
ggsave(filename = "Figures/initial_DO_timeseries_ordered_by_watershed.png",
       device = "png",
       dpi = 300,
       width = 16,
       height = 8,
       units = "in")

# Interactive dygraphs
# First need to get nest data
# df_dy_n <- df %>%
#   select(datetime, Subwatershed, Subwatershed_order, Site, DO, temp) %>%
#   group_by(Subwatershed) %>%
#   nest(.key = by_subwatershed) %>%
#   mutate(by_subwatershed = map(by_subwatershed, ~.x %>%
#                          group_by(Site, Subwatershed_order) %>%
#                          nest(.key = by_site)
#                        )
#          )

# First need to get data in correct timeseries format
# df_dy <- df_dy_n %>%
#   mutate(ts = map(by_subwatershed, "by_site") %>%
#            map_depth(2, ~zoo::zoo(x = c(.$DO, .$temp), order.by = .$datetime)),
#          )

# Create a graphing function
graph_fun <- function(data, site = "sitename") {
  if(site %in% c("Toranche St Cyr les Vignes",
                 "Coise aval Montrond",
                 "Lignon aval Poncins",
                 "Mare aval")){
  list(dygraph(data,
          main = site,
          width=800,height=200) %>%
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
    #        independentTicks = TRUE,
    #        valueRange = c(0, 14))  %>%
    dyAxis("y", label = "DO sat. (%)",
           independentTicks = TRUE,
           valueRange = c(0, 130))  %>%
    dyAxis("y2", label = "Temp", 
           valueRange = c(0, 30), 
           independentTicks = TRUE) %>%
    dySeries("temp", axis=('y2')) %>%
    dyRangeSelector(),
    
    
  } else {
    dygraph(data,
            main = site,
            width=800,height=200) %>%
      dyOptions(drawGrid = F,
                useDataTimezone = TRUE) %>%
      # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
      #        independentTicks = TRUE,
      #        valueRange = c(0, 14))  %>%
      dyAxis("y", label = "DO sat. (%)",
             independentTicks = TRUE,
             valueRange = c(0, 130))  %>%
      dyAxis("y2", label = "Temp", 
             valueRange = c(0, 30), 
             independentTicks = TRUE) %>%
      dySeries("temp", axis=('y2')) %>%
      dyRangeSelector()
  }
}

df_n <- df %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                        temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat) %>%
  select(Subwatershed, Location, Site, DO_per, temp, datetime) %>%
  group_by(Subwatershed, Location) %>%
  nest() %>%
  mutate(ts = map(data, ~zoo::zoo(x = .[, c("DO_per", "temp")], 
                                  order.by = .$datetime)),
         p = map2(ts, data, ~graph_fun(.x, unique(.y$Site)),
           ))

# df_dy2 <- df_dy %>%
#   mutate(p = map(by_subwatershed, "by_site") %>%
#            map_depth(2, ~zoo::zoo(x = ., order.by = .$datetime)) %>%
#            map_depth(2, ~graph_fun(.),
#   ))

pluck(df_dy2, 4, 2)

htmltools::browsable(htmltools::tagList(pluck(df_dy2, 4)))

htmltools::browsable(htmltools::tagList(pluck(df_n, 5)))

head(df_n)











test <- df_dy_n %>% 
  mutate(by_subwatershed = map(by_subwatershed, ~.x %>% 
                                 mutate(s = map(by_site,
                                                ~plot_ly(data = .x, 
                                                           x = ~datetime, 
                                                           y = ~DO,
                                                           type = "line"
                                                           )
                                                )
                                        )
  ))

t <- df_dy_n %>%
  mutate(dy = map_depth(by_subwatershed, 2, ~zoo::zoo(order.by = .$datetime)))
  

test %>% unnest(by_subwatershed) %>% slice(1:2)
test %>% unnest %>% filter(Site == "Toranche Pontet") %>% .$s
  map(~{
    plot_ly(data = .x, x = ~datetime, y = ~DO, type = "line")
  }) %>% 
  subplot(margin = .05)

  
  sol2<-nested_again %>% mutate(by_continent = map(by_continent, ~.x %>% 
                                                     mutate(models = map(by_country, ~lm(lifeExp ~ year, data = .x) )) )) 
  
  
mutate(df_dy_n, z = map(by_continent, "by_country") %>%
         at_depth(2, ~lm(lifeExp ~ year, data = .x)))




p <- mutate(df_dy_n, z = map(by_continent, "by_country") %>%
              at_depth(2, ~lm(lifeExp ~ year, data = .x)))



  mutate(data_s = map(data, spread()))
  spread(Site, DO, temp) %>%
  zoo::zoo(order.by = .$datetime)
df_dy$datetime <- NULL

dy_graphs <- list(
  dygraph(df_dy$belleville, 
          main = "Belleville",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$dampierre, 
          main = "Dampierre",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$chinon, 
          main = "Chinon",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)),
  
  dygraph(df_dy$vienne, 
          main = "Vienne",
          group = "df_dy",
          width=800,height=200) %>% 
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg L<sup>-1</sup>)", 
           independentTicks = TRUE,
           valueRange = c(0, 22)) %>%
    dyRangeSelector()
)

htmltools::browsable(htmltools::tagList(dy_graphs))