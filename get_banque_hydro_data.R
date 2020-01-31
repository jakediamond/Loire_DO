# 
# Purpose: To get all discharge data for Loire headwaters
# Author: Jake Diamond
# Date: October 10, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(tidyverse)

# Load meta data
meta <- read_xlsx("Data/Headwaters_DO/Discharge/Headwater_discharge_banque_hydro_codes.xlsx")

#Special get function
get_q <- function(code, start, end) {
  start_dt = paste0("01/01/", start, " 00:00")
  end_dt = paste0("20/09/", end, " 23:59")
  result = rbanqhydro.get(station = code, 
                 DateHeureDeb = start_dt, 
                 DateHeureFin = end_dt, 
                 procedure = "QTFIX")
  result
}

# Safe function
safe_get_q <- safely(get_q, otherwise = NA_real_)

df <- meta %>%
  mutate(q = map(Code, ~rbanqhydro.get(.x, 
                                             "01/03/2000 00:00", 
                                             "20/09/2019 23:59")))




df_q <- rbanqhydro.get("K0773220","01/01/1980 00:00", "20/09/2019 23:59")
df_q2 <- rbanqhydro.get("K0773220","01/01/1976 00:00", "20/09/1980 23:59")
df_q <- get_q("K0773220", "2001", "2019")

df_q <- transpose(df$q)
df_q2 <- pluck(df_q,2,2, 1)         

df_q3 <- rbanqhydro.get("K0704510","01/01/1977 00:00", "20/09/2019 23:59")


meta %>%
  group_by(Site) %>%
  summarize(start = seq(start_year, end_year - 4, 4))
