# Model
model = list(gpp ~ 1 + ar(1))

# Simulate data
empty = mcp(model, sample = FALSE, par_x = "time")
set.seed(42)  # For consistent "random" results
df = data.frame(time = 1:800)
df$gpp = empty$simulate(
  df$time, 
  int_1 = 10, 
  ar1_1 = 0.9, 
  sigma_1 = 3)
plot(df)
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
