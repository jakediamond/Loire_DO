

# Set working directory
setwd("Z:/Loire_DO")

# CCM
library(multispatialCCM)
library(zoo)
library(imputeTS)
library(rEDM)
library(plotly)
library(lubridate)
library(future)
library(tidyverse)

# Load data
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins") %>%
  mutate(month = month(date),
         year = year(date),
         GPP = ifelse(GPP < 0, NA, GPP),
         ER = ifelse(ER > 0, NA, ER),
         NEP = GPP + ER)

# Define groups
year_grps <- tibble(year = seq(1993, 2018, 1),
                    time = ntile(year, 5))

df_ccm_n <- df_met %>%
  filter(between(month, 5, 9)) %>%
  left_join(year_grps) %>%
  group_by(time) %>%
  # group_by(year) %>%
  nest()

# data <- pluck(df_ccm_n, 2, 2)
# plot(data$GPP)
# plot(Accm)
# plot(data$ER)
# x = pluck(df_ccm_n, 2, 5)[70:100, ]
# plot(x =x$date, y = x$GPP)
# points(x =x$date, y = -x$ER, col ="red" )
# chla <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
#   dplyr::filter(solute == "CHLA") %>%
#   group_by(year, month) %>%
#   summarize(chla = mean(value, na.rm = TRUE)) %>%
#   na.trim(.) %>%
#   ungroup() %>%
#   mutate(time = row_number()) %>%
#   select(time, chla) %>%
#   as.data.frame(.)
# chla$chla <- na_interpolation(chla$chla, option = "stine")
# chla_plot <- chla %>%
#   mutate(d1 = lag(chla), d2 = lag(chla, 2))
# plot_ly(chla_plot, x=~chla, y=~d1, z=~d2, type = 'scatter3d', mode = 'lines',
#         opacity = 1, line = list(width = 3, color = ~time, reverscale = FALSE))
# 
# 
# simplex_out <- Simplex(dataFrame = chla, lib = "1 100", pred = "201 462",
#                        columns = "chla", target = "chla", E = 3)
# simplex_out[c(1:2, 300:301), ]
# ComputeError(simplex_out$Observations, simplex_out$Predictions)
# 
# rho_E <- EmbedDimension(dataFrame = chla, lib = "1 100", pred = "201 462",
#                         columns = "chla", target = "chla")
# rho_Tp <- PredictInterval(dataFrame = chla, lib = "1 100", pred = "201 462",
#                           target = "chla", columns = "chla", E = 3)
# rho_theta <- PredictNonlinear(dataFrame = chla, lib = "1 100", pred = "201 462",
#                               target = "chla", columns = "chla", E = 3)
# make a ccm function
ccm_fun <- function(data){
  Accm = data %>%
    group_by(year) %>%
    nest() %>%
    transmute(clean = future_map(data, ~na_interpolation(na.trim(.$GPP)))) %>%
    map_dfr(rbind, NA) %>%
    unnest(clean) %>%
    ungroup(.) %>%
    select(clean) %>%
    # na_interpolation(.$GPP) %>%
    # na.trim(.) %>%
    # ungroup(.) %>%
    # select(GPP) %>%
    as.matrix(.)
  
  Bccm = data %>%
    group_by(year) %>%
    nest() %>%
    transmute(clean = future_map(data, ~na_interpolation(na.trim(.$ER)))) %>%
    map_dfr(rbind, NA) %>%
    unnest(clean) %>%
    ungroup(.) %>%
    select(clean) %>%
    # na_interpolation(.$ER) %>%
    # na.trim(.) %>%
    # ungroup(.) %>%
    # select(ER) %>%
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
  CCM_boot_A = CCM_boot(Accm, Bccm, E_A, tau=1, iterations=100)
  # Does B "cause" A?
  CCM_boot_B = CCM_boot(Bccm, Accm, E_B, tau=1, iterations=100)

  ctest = ccmtest(CCM_boot_A,CCM_boot_B)
  
  res = list(results = data.frame(a_cause_b = ctest[1],
                                  b_cause_a = ctest[2]),
             boota = data.frame(Lobs = CCM_boot_A$Lobs,
                                rho = CCM_boot_A$rho,
                                sdrho = CCM_boot_A$sdevrho),
             bootb = data.frame(Lobs = CCM_boot_B$Lobs,
                                rho = CCM_boot_B$rho,
                                sdrho = CCM_boot_B$sdevrho))
}

# ccm results
ccmres <- df_ccm_n %>%
  mutate(ccm = future_map(data, ccm_fun))
saveRDS(ccmres, "Data/Loire_DO/ccm_results_groupedyears")
ccmres <- readRDS("Data/Loire_DO/ccm_results_groupedyears")
test <- unnest_wider(ccmres, ccm)

resultsa <- test %>% 
  select(time, boota) %>%
  unnest(boota)

resultsb <- test %>% 
  select(time, bootb) %>%
  # unnest_longer(bootb) %>%
  unnest(bootb)
pvals <- test %>%
  select(time, results) %>%
  unnest(results)
ggplot(data = pivot_longer(pvals, cols = c(-time)),
       aes(x = time,
           y = value,
           color = name)) +
  # geom_jitter() +
  geom_line() +
  theme_bw()

ggplot() +
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
  facet_wrap(~time)






#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0.6,1),
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
Thrips3 = df_met %>%
  filter(between(month, 5, 9),
         between(year, 1998, 2004)) %>%
  na.trim(.) %>%
  as.data.frame(.) %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  na_interpolation(.)

df <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  dplyr::filter(solute %in% c("temp", "CHLA", "BOD5", "PO4")) %>%
  group_by(year, month, solute) %>%
  summarize_all(.funs = mean, na.rm = TRUE) %>%
  ungroup() %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  filter(year > 1992) %>%
  select(-site_no, -day, -date)

Thrips <- df_met %>%
  # filter(between(month, 5, 9),
  #        between(year, 1998, 2004)) %>%
  group_by(year, month) %>%
  summarize_all(.funs = mean, na.rm = TRUE) %>%
  na.trim(.) %>%
  right_join(df, by = c("year", "month")) %>%
  as.data.frame(.) %>%
  left_join(readRDS("Data/Discharge/dampierre_discharge_for_metab")) %>%
  na_interpolation(.)
Thrips <- Thrips[1:150, ]
Thrips <- Thrips[168:310, ]
  
  

rho_E <- EmbedDimension(dataFrame = Thrips, columns = "GPP", target = "ER",
                        lib = "1 120", pred = "1 120", showPlot = TRUE)
E = 10
rho_theta_e3 = PredictNonlinear(dataFrame = Thrips, columns = "GPP", target = "ER",
                                lib = "1 100", pred = "1 100", E = E)


vars = colnames(Thrips[c(4:5, 9:13)])
var_pairs = combn(vars, 2) # Combinations of vars, 2 at a time
libSize = paste(NROW(Thrips) - E, NROW(Thrips) - E, 10, collapse = " ")
ccm_matrix = array(NA, dim = c(length(vars), length(vars)), 
                   dimnames = list(vars,
                                   vars))
for (i in 1:ncol(var_pairs)) {
  ccm_out = CCM(dataFrame = Thrips, columns = var_pairs[1, i], 
                target = var_pairs[2,i], libSizes = libSize, 
                Tp = 0, E = E, sample = 100)
  outVars = names(ccm_out)
  var_out = unlist(strsplit(outVars[2], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
  var_out = unlist(strsplit(outVars[3], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
}

corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,
                                                                              vars))
for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(Thrips[, ccm_from], Thrips[, ccm_to], type = "correlation",
                   lag.max = 6, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out))
  }
}




thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = 5, Tp = 0, columns = "CHLA",
                        target = "GPP", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = 5, Tp = 0, columns = "discharge.daily",
                        target = "ER", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = 5, Tp = 0, columns = "BOD5",
                        target = "ER", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)
# abline(h = corr_matrix["Thrips_imaginis", "maxT_degC"], col = "black", lty = 2)
thrips_xmap <- CCM(dataFrame = Thrips2, E = 5, Tp = 0, columns = "discharge.daily",
                        target = "GPP", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)
thrips_xmap <- CCM(dataFrame = Thrips2, E = 5, Tp = 0, columns = "discharge.daily",
                        target = "ER", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)
thrips_xmap <- CCM(dataFrame = Thrips2, E = 5, Tp = 0, columns = "GPP",
                        target = "ER", libSizes = "10 100 5", sample = 300, 
                        showPlot = TRUE)

thrips_xmapmid <- CCM(dataFrame = Thrips3, E = 5, Tp = 0, columns = "discharge.daily",
                   target = "GPP", libSizes = "10 100 5", sample = 300, 
                   showPlot = TRUE)
thrips_xmapmid <- CCM(dataFrame = Thrips3, E = 5, Tp = 0, columns = "discharge.daily",
                   target = "ER", libSizes = "10 100 5", sample = 300, 
                   showPlot = TRUE)
thrips_xmapmid <- CCM(dataFrame = Thrips3, E = 5, Tp = 0, columns = "GPP",
                   target = "ER", libSizes = "10 100 5", sample = 300, 
                   showPlot = TRUE)

