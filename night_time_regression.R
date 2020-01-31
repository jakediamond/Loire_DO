

df_met <- df_DO %>%
  mutate(hour = hour(datetime),
         date = date(datetime),
         night = ifelse(hour>18 | hour<6, 1, 0),
         DOsat = ifelse(temp == 0,
                       0,
                       14.652 - 0.41022 * temp + 0.007991 * 
                         temp^2 - 0.000077774 * temp^3),
         dDO = lead(DO_use) - DO_use,
         sat_def = DOsat - DO_use)

library(broom)
df_k <- df_met %>%
  filter(night == 1) %>%
  mutate(date_use = ifelse(hour < 19, date - 1, date)) %>%
  group_by(date_use) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(.$dDO ~ .$sat_def)),
         res = map(mod, tidy)) %>%
  unnest(res)

ggplot(df_k,
       aes(x = date_use,
           y = estimate)) + 
  geom_point() +
  facet_wrap(~term, scales = "free_y")


library(streamMetabolizer)
metab_night()
metab_model(model_class = "night")



df_dy <- zoo::zoo(select(df_met, DO_obs, DO_use, filt), order.by = df_met$datetime)
dygraph(data = df_dy,
           width=800,height=200) %>%
 dyOptions(drawGrid = F,
  useDataTimezone = TRUE) %>%
 # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
  #        independentTicks = TRUE,
  #        valueRange = c(0, 14))  %>%
 dyAxis("y", label = "DO mg/l",
           independentTicks = TRUE)  %>%
  dySeries("DO_obs") %>%
  dySeries("DO_use") %>%
  dySeries("filt") %>%
  dyRangeSelector()
