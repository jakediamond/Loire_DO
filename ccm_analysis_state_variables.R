
# -------------------------------------
# Author: Jake Diamond
# Purpose: CCM analysis of state variables
# Date: May 7, 2020
# -------------------------------------

# Load libraries
devtools::install_github("ha0ye/rEDM")
library(rEDM)
library(multispatialCCM)
library(readxl)
library(tidyverse)
library(lubridate)

# Load data
df <- read_csv("Data/all_data_monthly_ccm_clean.csv")

# Select the embedding dimension ------------------------------------------
# Period 1
ts <- df$q
lib <- c(1, length(ts))
pred <- c(1, length(ts))
simplex_output <- simplex(ts, lib, pred, silent = TRUE)
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

# Period 2
ts2 <- df$gpp
lib2 <- c(1, length(ts2))
pred2 <- c(1, length(ts2))
simplex_output2 <- simplex(ts2, lib2, pred2, silent = TRUE)
plot(simplex_output2$E, simplex_output2$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

# Test for nonlinearity with S-mapping ------------------------------------
# Check two best embedding dimensions
# Period 1
smap_output <- s_map(ts, lib, pred, E = 3, silent = TRUE)

plot(smap_output$theta, smap_output$rho, type = "l", xlim = c(0, 4), 
     xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

# Period 2
smap_output2 <- s_map(ts2, lib2, pred, E = 3, silent = TRUE)

plot(smap_output2$theta, smap_output2$rho, type = "l", xlim = c(0, 4), 
     xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

# nonlinear models (theta > 0) give slightly better predictions than the 
# linear model (theta = 0) for both periods

# Two-part criterion for CCM to be a rigorous test of causality: (1) cross map 
# prediction skill is statistically significant when using the full time series 
# as the library. (2) cross map prediction skill demonstrates convergence - 
# prediction skill increases as more of the time series is used to reconstruct 
# the attractor.
# CCM part 1 ---------------------------------------------------------------------
# Get all the variable names
vars <- colnames(df[-1])

# Period 1
n <- NROW(df)
ccm_rho_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars),
                         dimnames = list(vars, 
                                         vars))
# Cross map skill for each variable pair
for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    out_temp <- ccm(df, E = 3, lib_column = ccm_from, 
                    target_column = ccm_to, 
                    lib_sizes = n, replace = FALSE, silent = TRUE)
    ccm_rho_matrix[ccm_from, ccm_to] <- out_temp$rho
  }
}

# For comparison we compute the lagged cross-correlation, allowing lags of up to ?6 months.
corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, 
                                                                              vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    cf_temp <- ccf(df[, ccm_from], df[, ccm_to], type = "correlation", 
                   lag.max = 3, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(cf_temp))
  }
}
head(ccm_rho_matrix)
head(corr_matrix)


# Period 2
n2 <- NROW(df[7301:9485,])
ccm_rho_matrix2 <- matrix(NA, nrow = length(vars), ncol = length(vars),
                          dimnames = list(vars, 
                                          vars))
# Cross map skill for each variable pair
for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    out_temp2 <- ccm(df[7301:9485,], E = 6, lib_column = ccm_from, 
                     target_column = ccm_to, 
                     lib_sizes = n2, replace = FALSE, silent = TRUE)
    ccm_rho_matrix2[ccm_from, ccm_to] <- out_temp2$rho
  }
}

# For comparison we compute the lagged cross-correlation, allowing lags of up to ?6 days.
corr_matrix2 <- array(NA, dim = c(length(vars), length(vars)), 
                      dimnames = list(vars, 
                                      vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    cf_temp2 <- ccf(df[7301:9426, ccm_from], df[7301:9426, ccm_to], 
                    type = "correlation", 
                    lag.max = 6, plot = FALSE)$acf
    corr_matrix2[ccm_from, ccm_to] <- max(abs(cf_temp2))
  }
}
head(ccm_rho_matrix2)
head(corr_matrix2)

# For both periods there is high correlation between temperature and GPP. This
# makes interpretation more complicated, because we have to consider the 
# possibility that cross mapping between temperature/GPP/ER abundance occurs 
# because of the shared seasonality. In other words, we may observe high 
# cross mapping ?? between two variables with a seasonal cycle, even if there 
# is no underlying causal mechanism.
# CCM part 2 --------------------------------------------------------------

# Look at convergence of cross map skill, compute rho as function of library size
# Period 1
p_xmap_chla <- ccm(df, E = 6, random_libs = TRUE, lib_column = "chla", 
                   target_column = "p", lib_sizes = seq(10, 130, by = 10), 
                   num_samples = 300, 
                   silent = TRUE,
                   tp = 0)
chla_xmap_p <- ccm(df, E = 6, random_libs = TRUE, lib_column = "chla", 
                   target_column = "p", lib_sizes = seq(10, 130, by = 10), 
                   num_samples = 300, 
                   silent = TRUE,
                   tp = 0)



a <- ccm_means(gpp_xmap_er)
b <- ccm_means(er_xmap_gpp)

plot(a$lib_size, pmax(0, a$rho), type = "l", 
     xlab = "Library Size", ylab = "Cross Map Skill (rho)", 
     col = "red", ylim = c(0, 1), lwd = 2)
lines(b$lib_size, pmax(0, b$rho), col = "blue", 
      lwd = 2)
abline(a = corr_matrix[5,6], b = 0, lty = "dashed")
legend(x = "topleft", col = c("red", "blue"), 
       lwd = 2, legend = c("p xmap chla", "chla xmap p"), 
       inset = 0.02, bty = "n", cex = 0.8)

# Corbicula xmap chlorophyll
cor_xmap_chla <- ccm(df, E = 6, random_libs = TRUE, lib_column = "cor", 
                   target_column = "chla", lib_sizes = seq(10, 130, by = 10), 
                   num_samples = 300, 
                   silent = TRUE,
                   tp = 0)
chla_xmap_cor <- ccm(df, E = 6, random_libs = TRUE, lib_column = "chla", 
                   target_column = "cor", lib_sizes = seq(10, 130, by = 10), 
                   num_samples = 300, 
                   silent = TRUE,
                   tp = 0)

cor_chla <- ccm_means(cor_xmap_chla)
chla_cor <- ccm_means(chla_xmap_cor)

plot(cor_chla$lib_size, pmax(0, cor_chla$rho), type = "l", 
     xlab = "Library Size", ylab = "Cross Map Skill (rho)", 
     col = "red", ylim = c(0.6, 1), lwd = 2)
lines(chla_cor$lib_size, pmax(0, chla_cor$rho), col = "blue", 
      lwd = 2)
abline(a = corr_matrix[4,5], b = 0, lty = "dashed")
legend(x = "topleft", col = c("red", "blue"), 
       lwd = 2, legend = c("cor xmap chla", "chla xmap cor"), 
       inset = 0.02, bty = "n", cex = 0.8)

# Corbicula xmap p
cor_xmap_p <- ccm(df, E = 6, random_libs = TRUE, lib_column = "cor", 
                     target_column = "p", lib_sizes = seq(10, 130, by = 10), 
                     num_samples = 300, 
                     silent = TRUE,
                     tp = 0)
p_xmap_cor <- ccm(df, E = 6, random_libs = TRUE, lib_column = "p", 
                     target_column = "cor", lib_sizes = seq(10, 130, by = 10), 
                     num_samples = 300, 
                     silent = TRUE,
                     tp = 0)

cor_p <- ccm_means(cor_xmap_p)
p_cor <- ccm_means(p_xmap_cor)

plot(cor_p$lib_size, pmax(0, cor_p$rho), type = "l", 
     xlab = "Library Size", ylab = "Cross Map Skill (rho)", 
     col = "red", ylim = c(0.6, 1), lwd = 2)
lines(p_cor$lib_size, pmax(0, p_cor$rho), col = "blue", 
      lwd = 2)
abline(a = corr_matrix[6,4], b = 0, lty = "dashed")
legend(x = "topleft", col = c("red", "blue"), 
       lwd = 2, legend = c("cor xmap p", "p xmap cor"), 
       inset = 0.02, bty = "n", cex = 0.8)

# Corbicula xmap p
cor_xmap_p <- ccm(df, E = 6, random_libs = TRUE, lib_column = "cor", 
                  target_column = "p", lib_sizes = seq(10, 130, by = 10), 
                  num_samples = 300, 
                  silent = TRUE,
                  tp = 0)
p_xmap_cor <- ccm(df, E = 6, random_libs = TRUE, lib_column = "p", 
                  target_column = "cor", lib_sizes = seq(10, 130, by = 10), 
                  num_samples = 300, 
                  silent = TRUE,
                  tp = 0)

cor_p <- ccm_means(cor_xmap_p)
p_cor <- ccm_means(p_xmap_cor)

plot(cor_p$lib_size, pmax(0, cor_p$rho), type = "l", 
     xlab = "Library Size", ylab = "Cross Map Skill (rho)", 
     col = "red", ylim = c(0.6, 1), lwd = 2)
lines(p_cor$lib_size, pmax(0, p_cor$rho), col = "blue", 
      lwd = 2)
abline(a = corr_matrix[6,4], b = 0, lty = "dashed")
legend(x = "topleft", col = c("red", "blue"), 
       lwd = 2, legend = c("cor xmap p", "p xmap cor"), 
       inset = 0.02, bty = "n", cex = 0.8)

# gpp xmap discharge
gpp_xmap_q <- ccm(df, E = 5, random_libs = TRUE, lib_column = "gpp", 
                  target_column = "q", lib_sizes = seq(10, 130, by = 10), 
                  num_samples = 300, 
                  silent = TRUE,
                  tp = 0)
q_xmap_gpp <- ccm(df, E = 4, random_libs = TRUE, lib_column = "q", 
                  target_column = "gpp", lib_sizes = seq(10, 130, by = 10), 
                  num_samples = 300, 
                  silent = TRUE,
                  tp = 0)

gpp_q <- ccm_means(gpp_xmap_q)
q_gpp <- ccm_means(q_xmap_gpp)

plot(gpp_q$lib_size, pmax(0, gpp_q$rho), type = "l", 
     xlab = "Library Size", ylab = "Cross Map Skill (rho)", 
     col = "red", ylim = c(0.2, 1), lwd = 2)
lines(q_gpp$lib_size, pmax(0, q_gpp$rho), col = "blue", 
      lwd = 2)
abline(a = corr_matrix[2,7], b = 0, lty = "dashed")
legend(x = "topleft", col = c("red", "blue"), 
       lwd = 2, legend = c("gpp xmap q", "q xmap gpp"), 
       inset = 0.02, bty = "n", cex = 0.8)
# MultispatialCCM ---------------------------------------------------------
df_n <- tibble(xmap = c("chlorophyll-a xmap P", 
                        "chlorophyll-a xmap Corbicula", 
                        "P xmap Corbicula", 
                        "GPP xmap discharge"),
               data = list(data.frame(p = df$p, chla = df$chla),
                           data.frame(cor = df$cor, chla = df$chla),
                           data.frame(cor = df$cor, p = df$p),
                           data.frame(q = df$q, gpp = df$gpp)))
  

ccm_fun <- function(data){
  Accm = data[,1] 
  
  Bccm = data[,2]
  
  maxE = 8 #Maximum E to test
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
  #Want to use a higher E than 2
  matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
          xlab="E", ylab="rho", lwd=2)
  legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")
  E_A = which(Emat[-1, 1] == max(Emat[-1, 1])) + 2
  E_B = which(Emat[-1, 2] == max(Emat[-1, 2])) + 2
  
  # Ensure that prediction skill decreases with increasing steps
  signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                                 predsteplist=1:20)
  plot(x = signal_A_out$predatout$predstep, y = signal_A_out$predatout$rho)
  signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                                 predsteplist=1:20)
  plot(x = signal_B_out$predatout$predstep, y = signal_B_out$predatout$rho)
  # Run the CCM test
  #E_A and E_B are the embedding dimensions for A and B.
  #tau is the length of time steps used (default is 1)
  #iterations is the number of bootsrap iterations (default 100)
  # Does A "cause" B?
  CCM_boot_A = CCM_boot(Accm, Bccm, max(E_A, E_B), tau=1, iterations=1000)
  # Does B "cause" A?
  CCM_boot_B = CCM_boot(Bccm, Accm, max(E_A, E_B), tau=1, iterations=1000)
  
  ctest = ccmtest(CCM_boot_A,CCM_boot_B)
  
  res = list(results = data.frame(a_cause_b = ctest[1],
                                  b_cause_a = ctest[2]),
             preds = data.frame(predstepa = signal_A_out$predatout$predstep, 
                                rhoa = signal_A_out$predatout$rho,
                                predstepb = signal_B_out$predatout$predstep, 
                                rhob = signal_B_out$predatout$rho),
             boota = data.frame(Lobs = CCM_boot_A$Lobs,
                                rho = CCM_boot_A$rho,
                                sdrho = CCM_boot_A$sdevrho),
             bootb = data.frame(Lobs = CCM_boot_B$Lobs,
                                rho = CCM_boot_B$rho,
                                sdrho = CCM_boot_B$sdevrho))
}

# ccm results
ccm_res <- df_n %>%
  mutate(ccm = future_map(data, ccm_fun))

# Save for easy access later
saveRDS(ccm_res, "Data/Loire_DO/ccm_results_state_variables")

# Unnest the data to plot it
ccmres <- unnest_wider(ccm_res, ccm)

resultsa <- ccmres %>% 
  select(xmap, boota) %>%
  unnest(boota)

resultsb <- ccmres %>% 
  select(xmap, bootb) %>%
  unnest(bootb)

pvals <- ccmres %>%
  select(xmap, results) %>%
  unnest(results)

# Plot it
ccm_states <- ggplot() +
  geom_line(data = resultsa,
            aes(x = Lobs,
                y = rho)) +
  geom_ribbon(data = resultsa,
              aes(x = Lobs, ymin = rho - sdrho, ymax = rho + sdrho),
              alpha = 0.5) +
  geom_line(data = resultsb,
            aes(x = Lobs,
                y = rho),
            color = "red") +
  geom_ribbon(data = resultsb,
              aes(x = Lobs, ymin = rho - sdrho, ymax = rho + sdrho),
              alpha = 0.5,
              fill = "red") +
  geom_text(data = pvals,
            aes(x = 65, y = 0.96,
                label = paste0("list(", word(pvals$xmap, 3), 
                               "~forces~", word(pvals$xmap, 1),
                               ",~p==", a_cause_b, ")")),
            parse = TRUE,
            size = 1.8) +
  geom_text(data = pvals,
            aes(x = 65, y = 0.90,
                label = paste0("list(", word(pvals$xmap, 1), 
                               "~forces~", word(pvals$xmap, 3),
                               ",~p==", b_cause_a, ")")),
            parse = TRUE,
            size = 1.8,
            color = "red") +
  facet_wrap(~xmap) +
  theme_bw(base_size = 7) +
  scale_y_continuous(limits = c(0, 1)) +
  ylab(expression("Pearson correlation coefficient,"~rho)) +
  xlab("library length, L")

ggsave(plot = ccm_states,
       filename = "Figures/Middle_Loire/supplementary/ccm_state_variables.png",
       device = "png",
       dpi = 300,
       width = 92,
       height = 92,
       units = "mm")
