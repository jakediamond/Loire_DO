# Analysis of errors
# 
# # Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(dygraphs)
library(tidyverse)
# From Alison Appling: It's possible to calculate "final" predictions of DO by 
# taking observed DO at each time t-1, adding the model's prediction of dO/dt 
# at that time, then adding the process error for that timestep to get the 
# "final" prediction at time t.
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab"))
df_err <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins_process_errors")

df_err <- df_err %>%
  mutate(dDO_mod = DO.mod - lag(DO.mod),
         err_proc = if_else(is.na(err_proc_iid_mean), 0, err_proc_iid_mean),
         final_pred = lag(DO.obs) + dDO_mod + (err_proc/ depth / 24),
         resid = DO.obs - final_pred)

df_err_summary <- df_err %>%
  group_by(date) %>%
  summarize(err_sum = sum(resid, na.rm = TRUE),
            err_abs = sum(abs(resid), na.rm = TRUE)) %>%
  left_join(df_met)

ggplot(df_err_summary,
       aes(x = log(discharge.daily),
           y = err_sum,
           color = month(date))) +
  geom_point() +
  scale_color_viridis_c()

ggplot(filter(df_err_summary, between(month(date),5, 6)),
       aes(x = date,
           y = err_abs,
           color = month(date))) +
  geom_point() +
  scale_color_viridis_c() +
  stat_smooth()

# Interactive dygraphs
# First need to get data as zoo
df_dy <- df_err %>%
  select(DO.obs, final_pred) %>%
  zoo::zoo(., order.by = df_err$solar.time)

# Create a graphing function
graph_dy <- 
    dygraph(df_dy,
            main = "Model comparison to observed",
            width=800,height=200) %>%
      dyOptions(drawGrid = F,
                useDataTimezone = TRUE) %>%
      # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
      #        independentTicks = TRUE,
      #        valueRange = c(0, 14))  %>%
      dyAxis("y", label = "DO obs",
             independentTicks = TRUE,
             valueRange = c(0, 25))  %>%
      dyAxis("y2", label = "DO mod", 
             valueRange = c(0, 25), 
             independentTicks = TRUE) %>%
      dySeries("final_pred", axis=('y2')) %>%
      dyRangeSelector()




htmltools::browsable(htmltools::tagList(graph_dy))
