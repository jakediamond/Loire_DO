# Load discharge data
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")

# Load middle loire water quality data
df <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  ungroup() %>%
  group_by(solute, year, month) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  filter(year > 1979) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  select(solute, date, value)

# Load metabolism data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER)
df_use <- left_join(df_cam, df_met, by = "date")
ggplot(data = filter(df_use,
                     between(month(date), 4, 10)),
       aes(x = `COP (%)` * `Q (m3/s)`,
           y = lead(GPP))) +
  geom_point() +
  scale_x_log10()


df2 <- left_join(df_met, df_q) %>%
  left_join(pivot_wider(df, names_from = solute, values_from = value))
ggplot(data = df2,
       aes(x = BOD5,
           y = ER,
           color= year(date)))+
  geom_point()


dfsum <- df2 %>%
  mutate(DOC = if_else(is.na(DOC), mean(DOC, na.rm = TRUE), DOC),
         POC = 32 * (CHLA + PheoP),
         TOC = POC + DOC,
         rxn = abs((ER) * 200 * 10000) * 14/32,
         trans = discharge.daily * ((POC + DOC)*1000),
         Da = rxn / trans,
         year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  summarize(Da = mean(Da, na.rm = TRUE),
            DOC = sum(DOC * discharge.daily))
ggplot(data = dfsum,
       aes(x = year,
           y = Da)) +
  geom_point() +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  annotation_logticks(
    base = 10,
    sides = "l") +
  ylab(expression("Mean annual Damköhler (ER:river C flux)")) +
  theme(axis.title.x = element_blank())
