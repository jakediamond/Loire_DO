



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


Accm <- na.interpolation(na.trim(df_met_clean$GPP[(120+365*25):(267+365*25)]))
Bccm <- na.interpolation(na.trim(-df_met_clean$ER[(120+365*25):(267+365*25)]))

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
E_A<-2
E_B<-2
signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                               predsteplist=1:10)
signal_A_out
signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                               predsteplist=1:10)
# Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
CCM_boot_A<-CCM_boot(Accm, Bccm, E_A, tau=1, iterations=25)
# Does B "cause" A?
CCM_boot_B<-CCM_boot(Bccm, Accm, E_B, tau=1, iterations=25)


ccmtest(CCM_boot_A,CCM_boot_B)
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
