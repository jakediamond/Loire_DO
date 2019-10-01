


# Load necessary packages
library(car)
library(MASS)
library(leaps)
# Load data
pred <- readRDS("Data/Headwaters_DO/all_wq_data_summary") %>%
  left_join(read_xlsx("Data/Headwaters_DO/regression_data.xlsx")) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  # select_if(~ !any(is.na(.))) %>%
  left_join(readRDS("Data/Headwaters_DO/buffer_land_use_regression")) %>%
  select_if(~ !(sum(is.na(.)) > 3))

dep <- readRDS("Data/Headwaters_DO/DO_summary")

# Get data into good form
df <- dplyr::select(dep, amp, min, site = Site) %>%
  left_join(pred) %>%
  ungroup() %>%
  dplyr::select(-site)
# new
df2 <- dplyr::select(df, 
                     -starts_with("DO_"),
                     -ends_with("1m"),
                     -ends_with("2m"),
                     -ends_with("5m"),
                     -ends_with("min"),
                     -ends_with("max"),
                     -ends_with("sd")) %>%
                     # min, temp_mean) %>%
#   drop_na() %>%
  as.data.frame()
summary(df2)
# Fit the full model with no transformations
model1 <- lm(df2)
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
df2$amp <- df2$amp^0.5

# Run new model (don't need to transform predictor variables)
model_trans <- lm(df2)
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
subsets <- regsubsets(df$amp ~ ., 
                      data = df2, nvmax = 4)
summary(subsets)

# Best subsets are E, SE, ECP, and SECP
subs <- tibble(mods = list(lm(amp ~ SO4_mean, data = df2),
             lm(amp ~ SO4_mean +  K_mean, data = df2),
             lm(amp ~ PO4_mean + SO4_mean +  K_mean, data = df2),
             lm(amp ~ temp_mean + PO4_mean + SO4_mean +  K_mean, data = df2)
             
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
adjr2 <- summary(subsets)$adjr2
msres <- (summary(subsets)$rss) / nrow(df2)
bic <- summary(subsets)$bic
press <- map_df(subs, ~PRESS_fun(.x))

# Assumptions met, check for outliers in y-space
mod <- lm(amp ~ SO4_mean +  K_mean, data = df2) # Final model
out.df <- data.frame(pred = predict(mod),
                     resid = rstudent(mod))
plot(mod)
summary(mod)



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

hlim <- (2 * 4 / 51)
plot(h,
     ylab = "Hat values")
abline(a = hlim, b = 0, col = "red")
with(subset(h.df, h >= hlim),
     text(Index, h, state, pos = 2, offset = 0.8))

# Check for HIPs
cooks <- cooks.distance(model_trans)
c.df <- data.frame(state = states$State,
                   c = cooks,
                   Index = 1:51)

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
vif(mod)

# Condition number
states$Pres.bin <- ifelse(states$Pres == "RED", 0 , 1)
mod.bin <- lm(inv_inc ~ Education + CO2 + Pres.bin, data = states)
library(perturb)
colldiag(mod.bin)