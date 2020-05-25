# Cumulative annual discharge vs cumlative annual gpp
df_met <- 
  # df_gp %>%
  readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER) 
df_all <- left_join(df_met, df_q)

df_cum <- df_all %>%
  mutate(month = month(date),
         year = year(date)) %>%
  # filter(between(month, 6, 7)) %>%
  group_by(year) %>%
  summarize(q_cum = sum(ifelse(
    between(month, 3, 5), discharge.daily, NA), na.rm = TRUE),
    GPP_cum = sum(ifelse(
      between(month, 4, 10), GPP, NA),  na.rm = TRUE),
      ER_cum = sum(ER, na.rm = TRUE),
      NEP_cum = sum(NEP, na.rm = TRUE)) %>%
      mutate(transition = if_else(year < 2012, "before", "after"))
    
    ggplot(data = df_cum,
           aes(x = q_cum,
               y = GPP_cum,
               color = year,
               shape = transition)) +
      geom_point() +
      stat_smooth(aes(group = transition), method = "lm") +
      scale_color_viridis_c() +
      theme_bw() +
      geom_text(aes(label = year))
    
    mod <- lm(GPP_cum ~ q_cum, data = df_cum)
    plot(residuals(mod))
    