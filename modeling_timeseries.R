library(mcp)
library(earlywarnings)
library(lmtest)
set.seed(42)  # For consistent "random" results
# Model
model_gpp = list(gpp ~ 1 + ar(1))

# Simulate data
empty_gpp = mcp(model_gpp, sample = FALSE, par_x = "time")
df = data.frame(time = 1:800)
df$gpp = empty_gpp$simulate(
  df$time, 
  int_1 = 10, 
  ar1_1 = 0.9, 
  sigma_1 = 1.5)
plot(df$time, df$gpp)
generic_ews(df$gpp, winsize = 33)
# Model ER as function of GPP
model_er <- list(er ~0 + gpp)
# Simulate data
empty_er = mcp(model_er, sample = FALSE)
df$er = empty_er$simulate(
  df$gpp, 
  gpp_1 = -1, 
  sigma_1 = 1.5)

df$nep = df$gpp + df$er

# Model ER as AR1 with similar magnitude to GPP, but not a function of GPP
model_er2 <- list(er ~1 + ar(1))
# Simulate data
empty_er2 = mcp(model_er2, sample = FALSE, par_x = "time")
df$er2 = empty_er2$simulate(
  df$time, 
  int_1 = -10, 
  ar1_1 = 0.9, 
  sigma_1 = 1.5)

df$nep2 <- df$gpp + df$er2

# Simple correlation of the two ER/GPP regimes
plot(df$gpp, df$er)
plot(df$gpp, df$er2)

# Granger causality of the two ER/GPP
grangertest(diff(df$er) ~ diff(df$gpp), order = 1)
grangertest(diff(df$er2) ~ diff(df$gpp), order = 1)
grangertest(df$er ~ df$gpp, order = 1)
grangertest(df$er2 ~ df$gpp, order = 1)

# Early warnings of the two
generic_ews(df$er, winsize = 33)
generic_ews(df$nep, winsize = 33)
generic_ews(df$er2, winsize = 33)
generic_ews(df$nep2, winsize = 33)

# Model ER changepoint from one to the other
df$predictor <- ifelse(df$time < 300, df$gpp, df$time -299)
model_er3 <- list(er ~ 0 + predictor,
                  ~ 1 + predictor + ar(1))
# Simulate data
empty_er3 = mcp(model_er3, sample = FALSE)
df$er3 = empty_er3$simulate(
  df$predictor, 
  predictor_1 = -1,
  predictor_2 = 0,
  int_1 = -10, 
  int_2 = -10,
  cp_1 = 0,
  ar1_1 = 0.0, 
  ar1_2 = 0.9,
  sigma = 1.5)
plot(df$time, df$er3)
df$nep3 <- df$gpp + df$er3
generic_ews(df$nep3, winsize = 10)
x <- c(df$nep, df$nep2)
generic_ews(x, winsize = 25)

# Model ER changepoint from one to the other
df$predictor <- ifelse(df$time < 300, df$gpp, df$time -299)
model_er4 <- list(er ~ 0 + predictor,
                  ~ 1 + predictor + ar(1, 1 + predictor))
# Simulate data
empty_er4 = mcp(model_er4, sample = FALSE)
df$er4 = empty_er4$simulate(
  df$predictor, 
  predictor_1 = -1,
  predictor_2 = 0,
  int_1 = -10, 
  int_2 = -10,
  cp_1 = 0,
  ar1_1 = 0.0, 
  ar1_2 = 0.9,
  ar1_predictor_2 = 0.5,
  sigma = 1.5)
plot(df$time, df$gpp)
df$nep4 <- df$gpp + df$er4
generic_ews(df$nep4, winsize = 10)



df$er <- -df$gpp + rnorm(200)
df$nep <- df$gpp + df$er
plot(df)
x <- generic_ews(df$nep, winsize = 33)
acf(df$nep)
df$er2 <- rnorm(200, -10, 3)
df$nep2 <- df$gpp + df$er2
generic_ews(na.trim(df$nep2), winsize = 33)
acf(df$nep)

df$er3 = empty$simulate(
  df$time, 
  int_1 = -10, 
  ar1_1 = 0.9, 
  sigma_1 = 3)
df$nep3 = df$gpp + df$er3
plot(df$er3)
y <- generic_ews(df$nep3, winsize = 33)
y$type <- "decoupled"
x$type <- "coupled"
z <- bind_rows(x,y)
  
(ggplot(data = z,
       aes(x = timeindex,
           y = ar1,
           color = type)) +
  geom_line(size = 3) +
  scale_color_manual(name = "",
                     values = c("dark green", "dark blue")) +
  theme(legend.position = c(0.8, 0.5))) %>%
  ggsave(filename = "Figures/Middle_Loire/AR1_example_coupling.tiff",
         dpi = 300,
         units = "mm",
         height = 100,
         width = 100)


model2 = list(gpp ~ 1 + sin(time) + ar(1),
              ~ 0 + sin(time) + ar(1))

sin((2*pi*(time-(6/del_t)) / (24/del_t)))
# Simulate data
empty2 = mcp(model2, sample = FALSE)
set.seed(42)  # For consistent "random" results
df2 = data.frame(time = 1:365)
df2$er = empty$simulate(
  df$time, 
  int_1 = 10,
  cp_1 = 100,
  ar1_1 = 0.5, 
  ar2_1 = 0.9,
  sigma_1 = 3)
plot(df2)



# 
df$par <- 0.5*(540+440*sin(2*pi*df$time/365-1.4))

# Model gpp as function of light
model_gpp_light = list(gpp_light ~ 0 + ar(1) + par)

# Simulate data
empty_gpp_light = mcp(model_gpp_light, sample = FALSE)
# df = data.frame(time = 1:800)
df$gpp_light = empty_gpp_light$simulate(
  df$par, 
  par_1 = 0.05, 
  ar1_1 = 0.9, 
  sigma_1 = 1.5)
plot(df$time, df$gpp_light)

# Model ER as function of GPP_light
model_er_light <- list(er_light ~ 0 + gpp_light)
# Simulate data
empty_er_light = mcp(model_er_light, sample = FALSE)
df$er_light = empty_er$simulate(
  df$gpp_light, 
  gpp_1 = -1, 
  sigma_1 = 1.5)
plot(df$time, df$er_light)

df$nepl <- df$gpp_light + df$er_light

# Model ER as AR1 with similar magnitude to GPPlight, but not a function of GPPlight
model_er_light2 <- list(er_light2 ~ 0 + ar(1) + par)
# Simulate data
empty_er_light2 = mcp(model_er_light2, sample = FALSE)
df$er_light2 = empty_er_light2$simulate(
  df$par, 
  par_1 = -0.05, 
  ar1_1 = 0.9, 
  sigma_1 = 1.5)
plot(df$time, df$er_light2)

# Simple correlation of the two ER/GPP regimes
plot(df$gpp_light, df$er_light)
plot(df$gpp_light, df$er_light2)

# Granger causality of the two ER/GPP
grangertest(diff(df$er) ~ diff(df$gpp), order = 1)
grangertest(diff(df$er2) ~ diff(df$gpp), order = 1)
grangertest(diff(df$er_light) ~ diff(df$gpp_light), order = 1)
grangertest(diff(df$er_light2) ~ diff(df$gpp_light), order = 1)
grangertest(df$er ~ df$gpp, order = 1)
grangertest(df$er2 ~ df$gpp, order = 1)

# Early warnings of the two
generic_ews(df$er, winsize = 33)
generic_ews(df$nep, winsize = 33)
generic_ews(df$er2, winsize = 33)
generic_ews(df$nepl, winsize = 33)
