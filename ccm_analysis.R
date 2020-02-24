



# CCM
library(multispatialCCM)
df_ccm_n <- df_met %>%
  ungroup() %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER,
         year = year(date),
         month = month(date)) %>%
  filter(between(month, 4, 10)) %>%
  # mutate(time = ntile(year, 5)) %>%
  # group_by(time) %>%
  group_by(year) %>%
  nest()
data <- pluck(df_ccm_n, 2, 5)
plot(data$GPP)
plot(Accm)
plot(data$ER)
# make a ccm function
ccm_fun <- function(data){
  Accm = data %>%
    # group_by(year) %>%
    # nest() %>%
    # transmute(clean = future_map(data, ~na_interpolation(.$GPP))) %>%
    # map_dfr(rbind, NA) %>%
    # unnest(clean) %>%
    na_interpolation(.$GPP) %>%
    na.trim(.) %>%
    ungroup(.) %>%
    select(GPP) %>%
    as.matrix(.)
  
  Bccm = data %>%
    # group_by(year) %>%
    # nest() %>%
    # transmute(clean = future_map(data, ~na_interpolation(-.$ER))) %>%
    # map_dfr(rbind, NA) %>%
    # unnest(clean) %>%
    na_interpolation(.$ER) %>%
    na.trim(.) %>%
    ungroup(.) %>%
    select(ER) %>%
    as.matrix(.)
  
  maxE = 5 #Maximum E to test
  #Matrix for storing output
  Emat = matrix(nrow=maxE-1, ncol=2)
  colnames(Emat) = c("A", "B")
  
  #Loop over potential E values and calculate predictive ability
  #of each process for its own dynamics
  for(E in 2:maxE) {
    #Uses defaults of looking forward one prediction step (predstep)
    #And using time lag intervals of one time step (tau)
    Emat[E-1,"A"]=SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
    Emat[E-1,"B"]=SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
  }
  
  #Look at plots to find E for each process at which
  #predictive ability rho is maximized
  matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
          xlab="E", ylab="rho", lwd=2)
  legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
  E_A = which(Emat[, 1] == max(Emat[, 1])) + 1
  E_B = which(Emat[, 2] == max(Emat[, 2])) + 1
  signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                                 predsteplist=1:20)
  signal_A_out
  signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                                 predsteplist=1:20)
  signal_B_out
  # Run the CCM test
  #E_A and E_B are the embedding dimensions for A and B.
  #tau is the length of time steps used (default is 1)
  #iterations is the number of bootsrap iterations (default 100)
  # Does A "cause" B?
  CCM_boot_A = CCM_boot(Accm, Bccm, E_A, tau=1, iterations=10)
  # Does B "cause" A?
  CCM_boot_B = CCM_boot(Bccm, Accm, E_B, tau=1, iterations=10)

  ctest = ccmtest(CCM_boot_A,CCM_boot_B)
  
  data.frame(a_cause_b = ctest[1],
             b_cause_a = ctest[2])
}

# ccm results
ccmres <- df_ccm_n %>%
  mutate(ccm = future_map(data, ccm_fun)) %>%
  unnest(ccm)

ggplot(data = pivot_longer(ccmres, cols = c(-year,-data)),
       aes(x = year,
           y = value,
           color = name)) +
  geom_jitter() +
  geom_line()



#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0.4,1),
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

legend("topleft",
       c("A causes B", "B causes A"),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n")










df_gc_new <- df_mid_wide %>%
  filter(site_no == "04048000") %>%

  mutate(time = ntile(year, 4)) %>%
  select(time, NO3, PO4, CHLA, temp) %>%
  na.trim(.) %>%
  na_interpolation(.) %>%
  group_by(time) %>%
  nest()







# CCM
df_ccm_n2 <- df_mid_clean %>%
  dplyr::filter(solute %in% c("CHLA")) %>%
  filter(between(month, 4, 10)) %>%
  group_by(solute, year) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  left_join(df_cor_sum, by = "year") %>%
  ungroup() %>%
  filter(year >= 2000) #%>%
  # pivot_wider(names_from = solute, values_from = value) %>%
  # arrange(year, month) %>%
  # mutate(time = ntile(year, 4)) %>%
  # select(time, year, month, NO3, PO4, CHLA, temp) %>%
  # filter(between(month, 4, 10)) %>%
  # mutate(time = ifelse(year < 1990,
  #                      1,
  #                      ifelse(year < 2000,
  #                             2,
  #                             ifelse(year < 2010,
  #                                    3,
  #                                    4)))) %>%
  # group_by(time) %>%
  # nest()

# make a ccm function
ccm_fun2 <- function(data){
  Accm = data %>%
    group_by(year) %>%
    nest() %>%
    transmute(clean = future_map(data, ~na_interpolation(.$NO3))) %>%
    map_dfr(rbind, NA) %>%
    unnest(clean) %>%
    na.trim() %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix(.)
  
  Bccm = data %>%
    group_by(year) %>%
    nest() %>%
    transmute(clean = future_map(data, ~na_interpolation(.$CHLA))) %>%
    map_dfr(rbind, NA) %>%
    unnest(clean) %>%
    na.trim() %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix(.)
  
  maxE = 3 #Maximum E to test
  #Matrix for storing output
  Emat = matrix(nrow=maxE-1, ncol=2)
  colnames(Emat) = c("A", "B")
  
  #Loop over potential E values and calculate predictive ability
  #of each process for its own dynamics
  for(E in 2:maxE) {
    #Uses defaults of looking forward one prediction step (predstep)
    #And using time lag intervals of one time step (tau)
    Emat[E-1,"A"]=SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
    Emat[E-1,"B"]=SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
  }
  
  #Look at plots to find E for each process at which
  #predictive ability rho is maximized
  # matplot(1:maxE, Emat, type="l", col=1:2, lty=1:2,
  #         xlab="E", ylab="rho", lwd=2)
  # legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
  E_A = which(Emat[, 1] == max(Emat[, 1], na.rm = TRUE)) + 1
  E_B = which(Emat[, 2] == max(Emat[, 2], na.rm = TRUE)) + 1
  # signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
  #                                predsteplist=1:10)
  # signal_A_out
  # signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
  #                                predsteplist=1:10)
  # Run the CCM test
  #E_A and E_B are the embedding dimensions for A and B.
  #tau is the length of time steps used (default is 1)
  #iterations is the number of bootsrap iterations (default 100)
  # Does A "cause" B?
  CCM_boot_A = CCM_boot(Accm, Bccm, 2, tau=1, iterations=50)
  # Does B "cause" A?
  CCM_boot_B = CCM_boot(Bccm, Accm, 2, tau=1, iterations=50)
  
  ctest = ccmtest(CCM_boot_A, CCM_boot_B)
  data.frame(a_cause_b = ctest[1],
             b_cause_a = ctest[2])
}

# ccm results
ccmres2 <- df_ccm_n2 %>%
  mutate(ccm = future_map(data, ccm_fun2)) %>%
  unnest(ccm)

plot(df_ccm_n2$value)
lines(df_ccm_n2$density)

Accm = df_ccm_n2 %>%
  select(density) %>%
  as.matrix(.) %>%
  log(.)

Bccm = df_ccm_n2 %>%
  select(value) %>%
  as.matrix(.)

maxE = 5 #Maximum E to test
#Matrix for storing output
Emat = matrix(nrow=maxE-1, ncol=2)
colnames(Emat) = c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]=SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]=SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
E_A = which(Emat[, 1] == max(Emat[, 1], na.rm = TRUE)) + 1
E_B = which(Emat[, 2] == max(Emat[, 2], na.rm = TRUE)) + 1
# signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
#                                predsteplist=1:10)
# signal_A_out
# signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
#                                predsteplist=1:10)
# Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
CCM_boot_A = CCM_boot(Accm, Bccm, 3, tau=1, iterations=100)
# Does B "cause" A?
CCM_boot_B = CCM_boot(Bccm, Accm, 2, tau=1, iterations=100)

ctest = ccmtest(CCM_boot_A, CCM_boot_B)
ctest
data.frame(a_cause_b = ctest[1],
           b_cause_a = ctest[2])




library(rEDM)
