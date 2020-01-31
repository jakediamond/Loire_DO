
library(zoo)
library(imputeTS)
library(dygraphs)

# First subset data to get rid of NAs at the front or back end
dam <- na.trim(dam)

# Then interpolate all NAs so that there are none with Stineman method
dam$DO_int <- na_interpolation(dam$DO.obs, option = "stine")

# Then determine anomalies/outliers with the gesd method
# Use a trend of 7 days to catch major anomalies more easily
dam <- dam %>%
  time_decompose(DO_int, frequency = "day", trend = "7 days") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose() %>%
  select(datetime, anomaly) %>%
  right_join(dam)

z <- dam %>%
  mutate(month = month(datetime),
         day = date(datetime),
         season = ifelse(month %in% c(6,7,8),
                         "summer",
                         ifelse(month %in% c(9,10,11),
                                "fall",
                                ifelse(month %in% c(12,1,2),
                                       "winter",
                                       "spring"))),
         ddo = DO.obs - lag(DO.obs)) %>%
  group_by(day) %>%
  mutate(amp = max(DO.obs, na.rm = TRUE) - min(DO.obs, na.rm = TRUE),
         amp = ifelse(is.infinite(amp), NA, amp)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarize(mean_amp = mean(amp, na.rm = TRUE),
            se_amp = sd(amp, na.rm = TRUE) / sqrt(n()),
            mean_ddo = quantile(abs(ddo), probs = 0.95, na.rm = TRUE))

# Make the data NA where there are big jumps, or just wrong data (anomalies)
data <- dam %>% 
  # rename(datetime = solar.time) %>%
  mutate(ddo = DO.obs - lag(DO.obs),
         DO = ifelse(abs(ddo) > 2.5 | 
                       is.na(ddo) | 
                       DO.obs <= 1.5 |
                       (anomaly == "Yes" &
                          !(month(datetime) %in% c(6,7,8,9))),
                     NA,
                     DO.obs
                     )
         )

# Define a lowpass function to smooth
lowpass_fun <- function(data, cutoff_frequency = 3) {
  library(signal)
  library(imputeTS)
  # Re-interpolate all NAs so that there are none with Stineman method
  data$DO_an_int <- na_interpolation(data$DO, option = "stine")
  # Order the data, just in case
  data <- data[with(data, order(datetime)),]
  # Sampling rate [s^-1]
  sr <- 1 / (as.numeric(data$datetime[2] - data$datetime[1]) * 60)
  # Nyquist frequency = half the sampling rate
  nyq <- sr / 2
  # Cutoff frequency (hours^-1)
  cutoff <- 1 / (cutoff_frequency * 60 * 60)
  # Normalized cutoff frequency for Butterworth filter
  W <- cutoff / nyq
  # Butterworth low-pass filter, digital, 2nd order
  myfilter <- butter(2, W, type = 'low', plane = 'z')
  # Forward-reverse filter to remove phase-shift 
  # associated with Butterworth filter (must be in vector-form)
  vec <- as.vector(data$DO_an_int)
  filtered <- filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(datetime)), ]
  rem <- sr / cutoff
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}

# Use the lowpass filter
x <- lowpass_fun(data, cutoff_frequency = 0.15)

bv_do2 <- x %>%
  select(DO.obs, filtered) %>%
  zoo::zoo(order.by = x$datetime)
dygraph(bv_do2, main = "Loire à Dampierre") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  # dyAxis("y2", label = "SC", independentTicks = TRUE) %>%
  # dySeries("DO_int", axis=('y')) %>%
  dySeries("DO.obs", axis=('y'), drawPoints = TRUE)



x$medfilt <- runmed(x$DO)
plot(y)



bv_do3 <- data %>%
  select(DO, DO.obs, DO_int) %>%
  zoo::zoo(order.by = data$datetime)
dygraph(bv_do3, main = "Loire à Dampierre") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "DO", independentTicks = TRUE) %>%
  dySeries("DO", axis=('y')) %>%
  dySeries("DO.obs", axis=('y'), drawPoints = TRUE) %>%
  dySeries("DO_int", axis=('y'), drawPoints = TRUE)
