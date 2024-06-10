# Load necessary libraries
library(forecast)
library(ggplot2)
library(tseries)


# Load AirPassengers dataset
data(AirPassengers)

# Plot the original data
ggplot(AirPassengers, aes(x=as.Date(time(AirPassengers)), y=AirPassengers)) +
  geom_line() +
  labs(title="Monthly Total Airline Passengers", x="Year", y="Passengers")

# Perform spectral analysis using periodogram
spectrum_analysis <- spectrum(AirPassengers, main="Spectral Analysis of AirPassengers Data")

# Plot the periodogram
plot(spectrum_analysis$freq, spectrum_analysis$spec, type="l",
     xlab="Frequency", ylab="Spectral Density",
     main="Spectral Density of AirPassengers Data")



## Example 2

# Download the Mauna Loa CO2 dataset
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
co2_data <- read.table(url, header = FALSE, skip = 72, fill = TRUE, na.strings = "-99.99")

# Preprocess the data
colnames(co2_data) <- c("year", "month", "decimal_date", "average", "interpolated", "trend", "days")
co2_data <- co2_data[complete.cases(co2_data$average), ]

# Create a time series object
co2_ts <- ts(co2_data$average, start = c(co2_data$year[1], co2_data$month[1]), frequency = 12)

# Plot the CO2 data
autoplot(co2_ts) +
  labs(title = "Monthly Average CO2 Concentration at Mauna Loa", x = "Year", y = "CO2 (ppm)")

# Generate a Moving Average (MA) series
ma_order <- 3
ma_series <- filter(co2_ts, rep(1/ma_order, ma_order), sides = 2)

# Remove NA values from MA series
ma_series <- na.omit(ma_series)

# Generate an Autoregressive (AR) series
ar_order <- 1
ar_series <- arima.sim(model = list(ar = 0.7), n = length(co2_ts))

# Plot the original, MA, and AR series
par(mfrow = c(3, 1))
plot(co2_ts, main = "Original CO2 Series", ylab = "CO2 (ppm)", xlab = "Time")
plot(ma_series, main = paste("Moving Average Series (order =", ma_order, ")"), ylab = "CO2 (ppm)", xlab = "Time")
plot(ar_series, main = paste("Autoregressive Series (order =", ar_order, ")"), ylab = "Value", xlab = "Time")

# Spectral analysis of MA series
spectrum_ma <- spectrum(ma_series, main = "Spectral Density of MA Series")

# Spectral analysis of AR series
spectrum_ar <- spectrum(ar_series, main = "Spectral Density of AR Series")

# Plot the periodograms
par(mfrow = c(1, 2))
plot(spectrum_ma$freq, spectrum_ma$spec, type = "l",
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Periodogram of MA Series")
plot(spectrum_ar$freq, spectrum_ar$spec, type = "l",
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Periodogram of AR Series")




