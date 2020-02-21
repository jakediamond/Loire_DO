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
  filter(between(month, 4, 10)) %>%
  group_by(year) %>%
  nest()
library(furrr)
# granger causality
df_gc <- df_met_n %>%
  mutate(gc_er_gpp = future_map(data, ~grangertest(diff(.$ER) ~ diff(.$GPP), 
                                                   order = 2))) %>%
  unnest(gc_er_gpp)

ggplot(data = na.omit(df_gc),
        aes(x = year,
            y = `Pr(>F)`)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 7) +
    scale_x_continuous(breaks = seq(1994, 2018, 2)) +
    xlab("") +
    ylab(expression(ER[summer]*`~`*GPP[summer]~Granger~causality~pval))
  
  
  
) %>%
  ggsave(filename = "Figures/Middle_Loire/granger_causality_er_gpp_pool.png",
         device = "png",
         dpi = 300,
         width = 9,
         height = 6,
         units = "cm")

ccf(df_nopool$GPP[486:637],-df_nopool$ER[486:637], 
    lag.max = 3)
x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
         lag.max = 3)
pluck(x, "acf", 4)
# cross correlation
df_ccf <- df_met_n %>%
  mutate(data_trim = future_map(data, na.trim),
         data_fill = future_map(data_trim, na.interpolation),
         ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
                                             lag.max = 3)),
         lag0 = future_map(ccf_ge, pluck, "acf", 4),
         lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
  unnest(lag0, lag1)

ggplot(data = df_ccf) +
  geom_point(aes(x = year, y = lag0)) +
  geom_point(aes(x = year, y = lag1), color = "red")

# CCM
library(multispatialCCM)
df_met_clean <- df_met %>%
  ungroup() %>%
  # dplyr::select(-time_frame) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER,
         year = year(date),
         month = month(date))


Accm <- na.interpolation(na.trim(df_met_clean$GPP[(120+365*3):(267+365*3)]))
Bccm <- na.interpolation(na.trim(-df_met_clean$ER[(120+365*3):(267+365*3)]))

maxE<-5 #Maximum E to test
#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]<-SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
E_A<-4
E_B<-3
signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                               predsteplist=1:10)
signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                               predsteplist=1:10)
# Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
CCM_boot_A<-CCM_boot(Accm, Bccm, E_A, tau=1, iterations=25)
CCM_boot_B<-CCM_boot(Bccm, Accm, E_B, tau=1, iterations=25)
ccmtest(CCM_boot_A,CCM_boot_B)
#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2)



x <- ccf(diff(df_nopool$GPP[486:637]),-diff(df_nopool$ER[486:637]), 
         lag.max = 3)

df_ccf <- df_met_n %>%
  mutate(data_trim = future_map(data, na.trim),
         data_fill = future_map(data_trim, na.interpolation),
         ccf_ge = future_map(data_fill, ~ccf(diff(.$GPP), diff(.$ER), 
                                             lag.max = 3)),
         lag0 = future_map(ccf_ge, pluck, "acf", 4),
         lag1 = future_map(ccf_ge, pluck, "acf", 3)) %>%
  unnest(lag0, lag1)

ggplot(data = df_ccf) +
  geom_point(aes(x = year, y = lag0)) +
  geom_point(aes(x = year, y = lag1), color = "red")