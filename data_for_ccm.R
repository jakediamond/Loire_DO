df_all <- df_met %>%
  select(year, month, GPP, ER) %>%
  filter(between(month, 6,10)) %>%
  group_by(year, month) %>%
  summarize(gpp = mean(GPP, na.rm = TRUE),
            er = mean(ER, na.rm = TRUE)) %>%
  full_join(df_cor) %>%
  # left_join(df_mac) %>%
  full_join(pivot_wider(df_wq, names_from = solute, values_from = value) %>%
              select(date, CHLA, PO4) %>%
              mutate(month = month(date),
                     year = year(date)) %>%
              filter(between(month,6,10)) %>%
              group_by(year, month) %>%
              summarize(chla = mean(CHLA, na.rm = TRUE),
                        p = mean(PO4, na.rm = TRUE))) %>%
  full_join(df_q %>%
              mutate(month = month(date),
                     year = year(date)) %>%
              filter(between(month,6,10)) %>%
              group_by(year, month) %>%
              summarize(q = mean(log(discharge.daily), na.rm = TRUE))) %>%
  filter(year > 1992)
write.csv(df_all, "Data/Loire_DO/all_data_monthly_ccm.csv")
df_cor <- read_xlsx("Data/Corbicula/Corbicules_EDF_Dampierre_amont_aval_brutes_1979-2018.xlsx") %>%
  select(date = Dates,
         station = Stations,
         corbicula = Corbicula,
         area = SURF.m2,
         dens = DENS.ind.m2) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarize(cor = mean(log(dens), na.rm = TRUE))
