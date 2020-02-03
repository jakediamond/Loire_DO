gpp_ts <- df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
gpp_ts <- ts(gpp_ts$GPP, frequency = 365)
stl_gpp = stl(gpp_ts, "periodic")
seasonal_stl_gpp   <- stl_gpp$time.series[,1]
trend_stl_gpp     <- stl_gpp$time.series[,2]
random_stl_gpp  <- stl_gpp$time.series[,3]

plot(gpp_ts)
plot(as.ts(seasonal_stl_gpp))
plot(trend_stl_gpp)
abline(a = 4.5, b = -0.04)
plot(random_stl_gpp)
plot(stl_gpp)

summary(lm(trend_stl_gpp ~ index(seasonal_stl_gpp)))

# Only look at summer?
gpp_sum_ts <- df_met %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NPP = GPP + ER) %>%
  dplyr::filter(between(month(date), 4, 10),
                year(date) < 2004) %>%
  ungroup() %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
gpp_sum_ts <- ts(gpp_sum_ts$GPP, frequency = 214)
stl_gpp_sum = stl(gpp_sum_ts, "periodic")
seasonal_stl_gpp_sum <- stl_gpp_sum$time.series[,1]
trend_stl_gpp_sum <- stl_gpp_sum$time.series[,2]
random_stl_gpp_sum <- stl_gpp_sum$time.series[,3]

plot(gpp_sum_ts)
plot(as.ts(seasonal_stl_gpp_sum))
plot(trend_stl_gpp_sum)
abline(a = 4.5, b = -0.04)
plot(random_stl_gpp_sum)
plot(stl_gpp_sum)
mk.test(trend_stl_gpp_sum)
summary(lm(trend_stl_gpp_sum ~ index(seasonal_stl_gpp_sum)))


# Discharge
q_ts <- df_q %>%
  dplyr::filter(between(date, ymd("1993-01-01"),
                        ymd("2018-12-31"))) %>%
  right_join(ungroup(df_met) %>%
               dplyr::select(date)) %>%
  ungroup() %>%
  dplyr::select(discharge.daily) %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
q_ts <- ts(q_ts[,1], frequency = 365)
stl_q <- stl(q_ts, "periodic")
seasonal_stl_q   <- stl_q$time.series[,1]
trend_stl_q     <- stl_q$time.series[,2]
random_stl_q  <- stl_q$time.series[,3]

plot(q_ts)
plot(as.ts(seasonal_stl_q))
plot(trend_stl_q)
abline(a = 4.5, b = -0.04)
plot(random_stl_q)
plot(stl_q)

summary(lm(trend_stl_q ~ index(seasonal_stl_q)))

# Get GPP and discharge trends in one dataframe
df_ts_p <- as.tibble(pluck(stl_q, "time.series")) %>%
  mutate(type = "Discharge",
         time = dplyr::row_number(),
         seasonal_scale = scale(seasonal),
         remainder_scale = scale(remainder),
         trend_scale = scale(trend)) %>%
  bind_rows(as.tibble(pluck(stl_gpp, "time.series")) %>%
              mutate(type = "GPP",
                     time = dplyr::row_number(),
                     seasonal_scale = scale(seasonal),
                     remainder_scale = scale(remainder),
                     trend_scale = scale(trend))) %>%
  left_join(mutate(gpp_ts, time = dplyr::row_number()) %>%
              dplyr::select(time, date)) %>%
  pivot_longer(c(seasonal_scale, remainder_scale, trend_scale))

df_ts_p_scale <- df_ts_p %>%
  scale()

(ggplot(df_ts_p,
        aes(x = date,
            y = value,
            color = type)) +
    geom_line() +
    facet_grid(rows = vars(name), scales = "free_y") + 
    scale_color_manual(name = "",
                       values = c("blue",
                                  "red")) +
    scale_x_date(date_breaks = "1 years",
                 limits = c(ymd("1993-01-01"),
                            ymd("2018-12-31")),
                 date_labels = "%Y") +
    geom_hline(yintercept = 0) +
    theme_bw(base_size=7) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor = element_blank())) %>%
  ggsave(filename = "Figures/Middle_Loire/decomposition_gpp_discharge.png",
         device = "png",
         dpi = 300,
         height = 9.2,
         width = 18.4,
         units = "cm")

library(trend)
mk.test(trend_stl_gpp)
partial.cor.trend.test(trend_stl_gpp, trend_stl_q, method="pearson")
partial.cor.trend.test(trend_stl_gpp, trend_stl_q, method="spearman")

partial.mk.test(trend_stl_gpp, trend_stl_q)

# Discharge summer
q_ts <- df_q %>%
  dplyr::filter(between(date, ymd("1993-01-01"),
                        ymd("2018-12-31")),
                between(month(date), 4, 10),
                year(date) < 2004) %>%
  right_join(ungroup(df_met %>%
                       dplyr::filter(between(date, ymd("1993-01-01"),
                                             ymd("2018-12-31")),
                                     between(month(date), 4, 10),
                                     year(date) < 2004) %>%
                       dplyr::select(date))) %>%
  ungroup() %>%
  dplyr::select(discharge.daily) %>%
  as.data.frame() %>%
  na_interpolation(., option = "stine") %>%
  as.data.frame()
q_ts <- ts(q_ts[,1], frequency = 214)
stl_q <- stl(q_ts, "periodic")
seasonal_stl_q   <- stl_q$time.series[,1]
trend_stl_q     <- stl_q$time.series[,2]
random_stl_q  <- stl_q$time.series[,3]

partial.cor.trend.test(trend_stl_gpp_sum, trend_stl_q, method="pearson")
partial.cor.trend.test(trend_stl_gpp_sum, trend_stl_q, method="spearman")

partial.mk.test(trend_stl_gpp_sum, trend_stl_q)


# simpler summer analysis
df_met_sum <- df_met %>%
  mutate(year = year(date),
         month = month(date),
         season = case_when(
           month %in% 9:11 ~ "Fall",
           month %in%  6:8  ~ "Summer",
           month %in%  3:5  ~ "Spring",
           TRUE ~ "Winter"),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER))
df_met_sum2 <- df_met_sum %>%
  group_by(year, season) %>%
  summarize(med_gpp = mean(GPP, na.rm = TRUE),
            med_er = mean(ER, na.rm = TRUE)) %>%
  filter(season == "Summer")
summary(lm(med_er ~ year, data = df_met_sum2))

(ggplot(data = filter(df_met_sum, season == "Summer"),
        aes(x = year,
            y = ER)) +
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
    stat_smooth(method = "lm", geom = "line"))







df_met_p <- df_met_sum %>%
  mutate(`P:R` = GPP / -ER) %>%
  pivot_longer(names_to = "flux",
               values_to = "value",
               cols = c(GPP, ER, `P:R`)) %>%
  group_by(year, month, flux) %>%
  summarize(med = median(value, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year,
                          month,
                          "01",
                          sep = "-")))
(ggplot(data = df_met_p,
        aes(x = date,
            y = med)) + 
    theme_bw() + geom_line() +
    scale_x_date(limits = c(ymd("1993-01-01"), ymd("2020-01-01")),
                 breaks = seq(ymd("1995-01-01"), ymd("2020-01-01"),
                              "5 years"),
                 minor_breaks = seq(ymd("1993-01-01"), ymd("2020-01-01"),
                                    "1 years"),
                 labels = date_format("%Y")) +
    facet_wrap(~flux, ncol = 1, scales = "free_y") + 
    theme(axis.title.x = element_blank()) +
    xlab("") +
    ylab(expression(atop("Flux", "(mg"~O[2]~m^2*d^{-1}*")")))) %>%
  ggsave(filename = "Figures/Middle_Loire/metab_monthly_median.png",
         device = "png",
         width = 6,
         height = 6,
         units = "in",
         dpi = 300)



df_met_q <- left_join(df_met, df_q)
df_met_q %>%
  mutate(year = year(date)) %>%
  filter(between(month(date), 4, 10)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(r = future_map_dbl(data, 
                             ~cor(.$discharge.daily, .$GPP, method = "kendall",
                                  use = "complete.obs"))) %>%
  ggplot(aes(x = year, y = r)) + geom_point() +geom_line()


library(strucchange)
?breakpoints
