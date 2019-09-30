
pred <- readRDS("Data/Headwaters_DO/all_wq_data_summary") %>%
  left_join(read_xlsx("Data/Headwaters_DO/regression_data.xlsx")) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  select_if(~ !any(is.na(.)))

dep <- readRDS("Data/Headwaters_DO/DO_summary")


# Load necessary packages
library(car)
library(MASS)
library(leaps)

# Get data into good form
df <- dplyr::select(dep, amp, min, site = Site) %>%
  left_join(pred)
# new
df2 <- dplyr::select(df, min, temp_max, NO3_max, SC_mean,
                     treatment_capacity, TP_max, forest, weirs) %>%
  drop_na() %>%
  as.data.frame()


# Fit the full model with no transformations
model1 <- lm(min ~ temp_max + SC_mean + NO3_max + TP_max + 
               treatment_capacity + forest + weirs, 
             data = df2)
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

# Transformation suggests lambda = 2, but we use -1 to round and for simplicity
states$inv_inc <- states$Income^trans

# Run new model (don't need to transform predictor variables)
model_trans <- lm(inv_inc ~ Sales + Education + CO2 + Pres, data = states)
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
subsets <- regsubsets(min ~ temp_mean + NO3_mean + forest + weirs, 
                      data = df2, nbest = 1)
summary(subsets)

# Best subsets are E, SE, ECP, and SECP
subs <- list(lm(inv_inc ~ Education, data = states),
             lm(inv_inc ~ Sales +  Education, data = states),
             lm(inv_inc ~ Education + CO2 + Pres, data = states),
             lm(inv_inc ~ Sales + Education + CO2 + Pres, data = states)
             
)

# Create function for calculating PRESS
PRESS_fun <- function(mod) {
  # calculate the predictive residuals
  pr <- residuals(mod)/(1 - lm.influence(mod)$hat)
  # calculate the PRESS
  PRESS <- sum(pr^2)
}

# Get all summary stats for best subsets
cp <- summary(subsets)$cp
adjr2 <- summary(subsets)$adjr2
msres <- (summary(subsets)$rss) / nrow(states)
bic <- summary(subsets)$bic
press <- ldply(subs, PRESS_fun)

# Assumptions met, check for outliers in y-space
mod <- lm(inv_inc ~ Education + CO2 + Pres, data = states) # Final model
out.df <- data.frame(state = states$State, 
                     pred = predict(mod),
                     resid = rstudent(mod))

plot(out.df$pred, out.df$resid,
     xlab = "Predicted",
     ylab = "Studentized Residuals"
)
with(subset(out.df, abs(resid) >= 2),
     text(pred, resid, state, pos = 4, offset = 0.8))
abline(0, 0, lty = "dashed")
abline(2, 0, col = "red")
abline(-2, 0, col = "red")

# Check for outliers in x-space
h <- hatvalues(model_trans)
h.df <- data.frame(state = states$State,
                   h = h,
                   Index = 1:51)
levels(h.df$state) <- c(levels(h.df$state), "DC")
h.df$state[h.df$state == "District of Columbia"] <- "DC"

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
vif(model_trans)

# Condition number
states$Pres.bin <- ifelse(states$Pres == "RED", 0 , 1)
mod.bin <- lm(inv_inc ~ Education + CO2 + Pres.bin, data = states)
library(perturb)
colldiag(mod.bin)