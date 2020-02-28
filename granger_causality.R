# Granger causality ER GPP ------------------------------------------------
# Granger causality ER GPP
df_nopool <- df_met
library(lmtest)
x <- grangertest(df_nopool$GPP[486:637],df_nopool$ER[486:637], order = 2)
grangertest(df_nopool$ER[486:637],df_nopool$GPP[486:637], order = 3)
grangertest(diff(df_nopool$GPP[486:637]),diff(df_nopool$ER[486:637]), order = 2)
grangertest(diff(df_nopool$ER[486:637]),diff(df_nopool$GPP[486:637]), order = 2)
grangertest(diff(df_nopool$K600.daily[486:637]), diff(df_nopool$ER[486:637]), order = 1)

grangertest(diff(df_nopool$GPP[9252:9404]),diff(df_nopool$ER[9252:9404]), order = 2)
grangertest(diff(df_nopool$ER[9252:94047]),diff(df_nopool$GPP[9252:9404]), order = 2)
grangertest(diff(df_nopool$K600.daily[9252:9404]), diff(df_nopool$GPP[9252:9404]), order = 2)
plot(df_nopool$GPP[1:365])
plot(diff(df_nopool$K600.daily[1:365]))


df_met_n <- df_met %>%
  ungroup() %>%
  # dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER,
         year = year(date),
         month = month(date)) %>%
  filter(between(month, 4, 9)) %>%
  group_by(year) %>%
  nest()
# library(furrr)
# granger causality
df_gc <- df_met_n %>%
  mutate(gc_er_gpp = future_map(data, ~grangertest(diff(.$ER) ~ diff(.$GPP), 
                                                   order = 1))) %>%
  unnest(gc_er_gpp)

ggplot(data = na.omit(df_gc),
        aes(x = year,
            y = `Pr(>F)`)) +
    geom_point() +
  # geom_point(data = filter(df_mid_clean, solute == "CHLA") %>%
  #              group_by(year) %>%
  #              summarize(chla = quantile(value, 0.8, na.rm = TRUE) / 100),
  #            aes(x = year, y = chla),
  #            color = "darkgreen") +
    geom_line() +
    theme_bw(base_size = 7) +
    scale_x_continuous(breaks = seq(1994, 2018, 2),
                       limits = c(1994, 2018)) +
    xlab("") +
    ylab(expression(ER[summer]*`~`*GPP[summer]~Granger~causality~pval))
  

  
) %>%
  ggsave(filename = "Figures/Middle_Loire/granger_causality_er_gpp_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")

# ccf(df_nopool$GPP[486:637],-df_nopool$ER[486:637], 
#     lag.max = 3)
# x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
#          lag.max = 3)
# pluck(x, "acf", 4)
# cross correlation
# df_ccf <- df_met_n %>%
#   mutate(data_trim = future_map(data, na.trim),
#          data_fill = future_map(data_trim, na.interpolation),
#          ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
#                                              lag.max = 3)),
#          lag0 = future_map(ccf_ge, pluck, "acf", 4),
#          lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
#   unnest(lag0, lag1)

# ggplot(data = df_ccf) +
#   geom_point(aes(x = year, y = lag0)) +
#   geom_point(aes(x = year, y = lag1), color = "red")
# 
# 
# 
# x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
#          lag.max = 3)
# 
# df_ccf <- df_met_n %>%
#   mutate(data_trim = future_map(data, na.trim),
#          data_fill = future_map(data_trim, na.interpolation),
#          ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
#                                              lag.max = 3)),
#          lag0 = future_map(ccf_ge, pluck, "acf", 4),
#          lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
#   unnest(lag0, lag1)
# 
# ggplot(data = df_ccf) +
#   geom_point(aes(x = year, y = lag0)) +
#   geom_point(aes(x = year, y = lag1), color = "red")



# library(MTS)
df_gc_new <- df_mid_wide %>%
  filter(site_no == "04048000") %>%
  mutate(time = ntile(year, 4)) %>%
  select(time, NO3, PO4, CHLA, temp) %>%
  na.trim(.) %>%
  na_interpolation(.) %>%
  group_by(time) %>%
  nest()
plot(pluck(df_gc_new, 2, 1)$PO4)
df_gc2 <- grangertest(diff(df_gc_new$CHLA) ~ diff(df_gc_new$NO3), order = 1)
df_gc2
# granger causality
df_gc2 <- df_gc_new %>%
  mutate(gc_chla_po4 = future_map(data, ~grangertest(.$CHLA ~ .$PO4, 
                                                   order = 1))) %>%
  unnest(gc_chla_po4)

ggplot(data = na.omit(df_gc2),
       aes(x = time,
           y = `Pr(>F)`)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 7) +
  # scale_x_continuous(breaks = seq(1994, 2018, 2)) +
  xlab("") +
  ylab(expression(ER[summer]*`~`*GPP[summer]~Granger~causality~pval))  
  
  
gc_dat <- 
  mutate_all(.funs = diff) %>%
  as.matrix(.)

summary(GrangerTest(df_gc_new))



test2 <- df_met_n %>%
  mutate(dat = future_map(data, ts_fun2),
         ar1 = future_map(dat, ~acf(.$NEP, lag.max = 1))) %>%
  unnest(ar1)


ts_fun2 <- function(data){
  data = data %>%
    arrange(date) %>%
    # mutate(ind = row_number()) %>%
    na.trim() %>%
    as.data.frame(.)
  dat_ts = xts(x = data[, "NEP"],
               order.by = data[, "date"])
  dat_ts = na.trim(na_interpolation(dat_ts, option = "stine"))
  # dat_ts = na.trim(diff(diff(dat_ts), lag = 180))
}
