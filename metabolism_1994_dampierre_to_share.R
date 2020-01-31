# 
# Purpose: To estimate metabolism at Dampierre in 1993-2000
# Author: Jake Diamond
# Date: September 10, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
install.packages("streamMetabolizer", dependencies=TRUE, 
                 repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
devtools::install_github('USGS-R/streamMetabolizer')
# Load libraries
library(readxl)
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

# Read in data -----------------------------------------------------
df <- readRDS("dampierre_prepared_1994")
df_q <-readRDS("dampierre_prepared_1994_discharge")
# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a Bayesian model with both observation error and process error
# We will pool K600
bayes_mod <- mm_name(type = 'bayes', 
                      pool_K600 = 'binned', 
                      err_obs_iid = TRUE, 
                      err_proc_iid = TRUE)
bayes_mod

# Metabolism preparation---------------------------------------
# Calculate the natural-log-space centers of the discharge bins
# These are the bins for the time frame of 
# Use the width method as in the help file with with = 0.8 log units
brks <- calc_bins(vec = log(df_q$discharge.daily),
                  method = "width",
                  width = 0.8)$bounds

# Estimate the mean ln(k600) value for the river from O'Connor and direct 
# measurements with floating dome
# Theses are the hyperprior mean for k600 in log space 
k6 <- 0.19

# Same for standard deviation
k6_sd <- 0.22

# Set the specifications
bayes_specs <- specs(model_name = bayes_mod,
                     burnin_steps = 100,
                     saved_steps = 100
                     , K600_lnQ_nodes_centers = brks
                     , K600_lnQ_nodes_meanlog = rep(k6, 
                                                    length(brks))
                     , K600_lnQ_nodes_sdlog = rep(k6_sd, 
                                                  length(brks))
)

# Do the metabolism -------------------------------------------------------
mm <- metab(specs = bayes_specs, 
            data = as.data.frame(df), 
            data_daily = as.data.frame(df_q))

# Inspect the model -------------------------------------------------------
met_pred <- predict_metab(mm)
plot_metab_preds(mm)
ggplot(get_params(mm), aes(x=date, y=K600.daily)) + geom_point()
plot_distribs(bayes_specs, 'K600_lnQ_nodes')
get_fit(mm)$KQ_binned %>% select(index, lnK600_lnQ_nodes_2.5pct, lnK600_lnQ_nodes_50pct, lnK600_lnQ_nodes_97.5pct)
