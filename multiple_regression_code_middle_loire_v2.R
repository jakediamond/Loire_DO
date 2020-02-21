# Purpose: To regress variables with GPP/ER, for middle Loire River
# Author: Jake Diamond
# Date: January 10, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(scales)
library(furrr)
library(patchwork)
library(car)
library(MASS)
library(readxl)
library(leaps)
library(tidyverse)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# # Set the plotting theme
theme_set(theme_bw(base_size=7)+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load some data
df_wq <- readRDS("Data/Loire_DO/middle_loire_wq") %>%
  pivot_wider(names_from = solute, values_from = value)
df_q <- readRDS("Data/Discharge/dampierre_discharge_for_metab")
df_met <- readRDS("Data/Loire_DO/metab_extremelyconstrainedK_gppconstrained_all_discharge_bins")
df_light <- readRDS("Data/Loire_DO/light_dampierre_for_metab") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(max_light = max(light, na.rm = TRUE),
            med_light = sum(light, na.rm = TRUE))
df_t <- readRDS("Data/Loire_DO/temp_median_dampierre")
df_use <- df_wq %>%
  left_join(df_q) %>%
  left_join(df_met) %>%
  left_join(df_light) %>%
  left_join(ungroup(df_t)) %>%
  ungroup() %>%
  filter(between(month, 4, 9)) %>%
  dplyr::select(date, GPP, ER, K600.daily, discharge.daily, max_light, med_light,
         temp, DOC, NO3, PO4, CHLA, SPM) %>%
  mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>%
  distinct(date, .keep_all = TRUE)

df_use


df_use_subs <- df_use %>%
  # mutate(tf = if_else(year(date) > 2005, "post", "pre")) %>%
  drop_na() %>%
  dplyr::select(-date, -max_light, -ER) %>%
  as.data.frame()

# Scaled data
scaled_df <- scale(df_use_subs) %>%
  as.data.frame()

# Interactions
fit_int <- lm(GPP ~ discharge.daily * temp * SPM, data = scaled_df)
summary(fit_int)
interact_plot(fit_int, pred = med_light, modx = SPM, linearity.check = TRUE,
              partial.residuals = TRUE, plot.points = TRUE)

regsubsets.out <-
  regsubsets(GPP ~ K600.daily + discharge.daily + max_light + temp + DOC + NO3 + PO4 + CHLA,
             data = df_use_subs,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

layout(matrix(1:1, ncol = 1))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)







# Fit the full model with no transformations
model1 <- lm(df_use_subs)
summary(model1)

# Check Assumptions
m1_resids <- rstudent(model1) #Studentized residuals for model 2
m1_pred <- predict(model1) #Predicted values for model 2
plot(m1_pred, m1_resids, #Plot the residuals vs predicted
     main = "Full model residual vs predicted",
     xlab = "Predicted",
     ylab = "Residuals"
)
qqnorm(m1_resids)
qqline(m1_resids)

# Conduct transformation
bc <- boxcox(model1)
trans <- bc$x[which.max(bc$y)]

# Transformation suggests lambda = 0.3, but we use 0.5 to round and for simplicity
df_use_subs$GPP <- df_use_subs$GPP^0.5

# Run new model (don't need to transform predictor variables)
model_trans <- lm(df_use_subs)
summary(model_trans)

# Check assumptions
mtrans_resids <- rstudent(model_trans) #Studentized residuals for transformed model
mtrans_pred <- predict(model_trans) #Predicted values for transformed model
plot(mtrans_pred, mtrans_resids, #Plot the residuals vs predicted
     main = "Transformed model residual vs predicted",
     xlab = "Predicted",
     ylab = "Residuals"
)
qqnorm(mtrans_resids)
qqline(mtrans_resids)

# Best subsets procedure
subsets <- regsubsets(GPP ~ ., 
                      data = df_use_subs, nvmax = 6)
summary(subsets)

# Best subsets are E, SE, ECP, and SECP
subs <- tibble(mods = list(lm(GPP ~ discharge.daily, data = df_use_subs),
                           lm(GPP ~ discharge.daily + temp, data = df_use_subs),
                           lm(GPP ~ discharge.daily + temp + max_light, data = df_use_subs),
                           lm(GPP ~ discharge.daily + temp + CHLA + SPM, data = df_use_subs),
                           lm(GPP ~ discharge.daily + temp + CHLA + SPM + max_light, data = df_use_subs),
                           lm(GPP ~ discharge.daily + temp + CHLA + SPM + max_light + DOC, data = df_use_subs)
                           
))

# Create function for calculating PRESS
PRESS_fun <- function(mod) {
  # calculate the predictive residuals
  pr <- residuals(mod)/(1 - lm.influence(mod)$hat)
  # calculate the PRESS
  PRESS <- sum(pr^2)
  return(PRESS)
}

# Get all summary stats for best subsets
cp <- summary(subsets)$cp
cp
adjr2 <- summary(subsets)$adjr2
adjr2
msres <- (summary(subsets)$rss) / nrow(df_use_subs)
bic <- summary(subsets)$bic
bic
press <- map_df(subs, PRESS_fun)

# Assumptions met, check for outliers in y-space
mod <- lm(GPP ~ discharge.daily + temp + CHLA + SPM + max_light + DOC, data = df_use_subs) # Final model
out.df <- data.frame(pred = predict(mod),
                     resid = rstudent(mod))
mod2 <- lm(GPP ~ discharge.daily + temp + max_light, data = df_use_subs)
library(jtools)
summ(mod, scale = TRUE, vifs = TRUE)
effect_plot(mod, pred = CHLA, interval = TRUE, plot.points = TRUE)
plot_summs(mod, mod2, scale = TRUE)
export_summs(mod, scale = TRUE, vifs = TRUE,
             error_pos = "right", ci_level = 0.95,
             statistics = "all",
             to.file = "xlsx", file.name = "Data/Loire_DO/mult_reg.xlsx")

plot(mod)
summary(mod)
mod_scale <-lm(GPP ~ discharge.daily + temp + CHLA + SPM + max_light + DOC, data = scaled_df)
plot(mod_scale)
summary(mod_scale)

plot(out.df$pred, out.df$resid,
     xlab = "Predicted",
     ylab = "Studentized Residuals"
)
with(subset(out.df, abs(resid) >= 2),
     text(pred, resid, pos = 4, offset = 0.8))
abline(0, 0, lty = "dashed")
abline(2, 0, col = "red")
abline(-2, 0, col = "red")

# Check for outliers in x-space
h <- hatvalues(model_trans)
# h.df <- data.frame(
#                    h = h,
#                    Index = 1:51)
# levels(h.df$state) <- c(levels(h.df$state), "DC")
# h.df$state[h.df$state == "District of Columbia"] <- "DC"

hlim <- (2 * 6 / 189)
plot(h,
     ylab = "Hat values")
abline(a = hlim, b = 0, col = "red")
with(subset(h.df, h >= hlim),
     text(Index, h, state, pos = 2, offset = 0.8))

# Check for HIPs
cooks <- cooks.distance(model_trans)
c.df <- data.frame(state = df_use_subs$GPP,
                   c = cooks)

plot(cooks,
     ylab = "Cook's Distance")
abline(1, 0, col = "red")
with(subset(c.df, c >= 1),
     text(Index, c, state, pos = 4, offset = 0.8))

# Remove DC and see what happens
df.nodc <- states[states$State != "District of Columbia", ]

# Re-run analysis without DC
mod.nodc <- lm(inv_inc ~ Education + CO2 + Pres, data = df.nodc) #Fit model
summary(mod.nodc)

# Check for multicollinearity
vif(model_trans)

# Condition number
states$Pres.bin <- ifelse(states$Pres == "RED", 0 , 1)
mod.bin <- lm(inv_inc ~ Education + CO2 + Pres.bin, data = states)
library(perturb)
colldiag(mod.bin)