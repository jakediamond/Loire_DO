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
library(dygraphs)
library(htmltools)
# library(ggpubr)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)



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

df <- df %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                          temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat)
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_filt <- df %>%
  filter(!(Site %in% flood_sites1 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-29 16:45"))),
         !(Site %in% flood_sites2 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-30 10:30"))),
         !(Site == "Coise aval Montrond" & between(datetime, 
                                                   ymd_hm("2019-08-06 16:00"),
                                                   ymd_hm("2019-09-11 11:30"))),
         !(Site == "Loise amont Doise Salt" & between(datetime, 
                                                      ymd_hm("2019-07-16 00:30"),
                                                      ymd_hm("2019-07-20 20:00"))),
         !(Site == "Loise Essertine en Donzy" & between(datetime, 
                                                        ymd_hm("2019-07-20 22:15"),
                                                        ymd_hm("2019-07-23 15:30"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                     ymd_hm("2019-07-08 19:00"),
                                                     ymd_hm("2019-07-16 14:15"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                     ymd_hm("2019-07-24 23:00"),
                                                     ymd_hm("2019-08-29 15:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-12 19:45"),
                                               ymd_hm("2019-07-21 17:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-23 22:45"),
                                               ymd_hm("2019-07-29 01:00"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-31 03:45"),
                                               ymd_hm("2019-08-06 17:30"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-07-14 15:00"),
                                     ymd_hm("2019-07-20 17:15"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-08-03 20:15"),
                                     ymd_hm("2019-08-06 12:30")))
         
  )

# Plot
p <- ggplot() +
  geom_line(data = df, aes(x = datetime,
                  y = DO_per,
                  color = as.factor(Watershed_order))) +
  # geom_line(data = df, aes(x = datetime,
  #               y = a + temp * b,
  #               color = Subwatershed_order),
  #           linetype = "dashed") +
  scale_x_datetime(breaks = xbrks,
                   date_labels = "%d") +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, 50),
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
df_dy_n <- df %>%
  select(datetime, Subwatershed, Subwatershed_order, Site, DO, temp) %>%
  group_by(Subwatershed) %>%
  nest() %>%
  mutate(by_subwatershed = map(data, ~.x %>%
                         group_by(Site, Subwatershed_order) %>%
                         nest()
                       )
         )

# First need to get data in correct timeseries format
df_dy <- df_dy_n %>%
  mutate(ts = map(by_subwatershed, "data") %>%
           map_depth(2, ~zoo::zoo(x = c(.$DO, .$temp), order.by = .$datetime)),
         )

# Create a graphing function
graph_fun <- function(data, site = "sitename") {
  # if(site %in% c("Toranche St Cyr les Vignes",
  #                "Coise aval Montrond",
  #                "Lignon aval Poncins",
  #                "Mare aval")){
  # dygraph(data,
  #         main = site,
  #         width=800,height=200) %>%
  #   dyOptions(drawGrid = F,
  #             useDataTimezone = TRUE) %>%
  #   # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
  #   #        independentTicks = TRUE,
  #   #        valueRange = c(0, 14))  %>%
  #   dyAxis("y", label = "DO sat. (%)",
  #          independentTicks = TRUE,
  #          valueRange = c(0, 130))  %>%
  #   dyAxis("y2", label = "Temp", 
  #          valueRange = c(0, 30), 
  #          independentTicks = TRUE) %>%
  #   dySeries("temp", axis=('y2')) %>%
  #   dyRangeSelector()
  #   
  #   
  # } else {
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
             valueRange = c(0, 160))  %>%
      dyAxis("y2", label = "Temp", 
             valueRange = c(0, 30), 
             independentTicks = TRUE) %>%
      dySeries("temp", axis=('y2')) %>%
      dyRangeSelector()
  # }
}

df_n <- df %>%
  mutate(DOsat = ifelse(temp == 0,
                        0,
                        14.652 - 0.41022 * temp + 0.007991 * 
                        temp^2 - 0.000077774 * temp^3),
         DO_per = DO * 100/ DOsat) %>%
  filter(Year == 2020) %>%
  arrange(Watershed, Subwatershed, Subwatershed_order) %>%
  select(Subwatershed, Location, Site, DO_per, temp, datetime) %>%
  filter(Subwatershed %in% c("Coise", "Coizet", "Potenisinet", "Toranche",
                             "Loise", "Doise", "Moulin Piquet", "Fontbonne",
                             "Charpassonne", "Carrat", "Rieu", "Violay")) %>%
  group_by(Subwatershed, Location) %>%
  nest() %>%
  mutate(ts = map(data, ~zoo::zoo(x = .[, c("DO_per", "temp")], 
                                  order.by = .$datetime)),
         p = map2(ts, data, ~graph_fun(.x, unique(.y$Site)),
           ))

df_dy2 <- df_dy %>%
  mutate(p = map(by_subwatershed, "data") %>%
           map_depth(2, ~zoo::zoo(x = ., order.by = .$datetime)) %>%
           map_depth(2, ~graph_fun(.),
  ))

pluck(df_dy2, 5, 2)

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