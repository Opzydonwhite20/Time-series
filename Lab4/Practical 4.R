# Load necessary libraries

library(ggplot2)
library(forecast)
library(tseries)

# Load AirPassengers dataset
data(AirPassengers)

# Plot the original data
ggplot(AirPassengers, aes(x=as.Date(time(AirPassengers)), y=AirPassengers)) +
  geom_line() +
  labs(title="Monthly Total Airline Passengers", x="Year", y="Passengers")

# Fourier series decomposition using the Fourier terms
fourier_terms <- fourier(ts(AirPassengers, frequency=12), K=5)  # Using K=5 for five harmonics

# Fit a linear model with Fourier terms
model_fourier <- lm(AirPassengers ~ fourier_terms)
summary(model_fourier)

# Spectral analysis using periodogram
spectrum_analysis <- spectrum(AirPassengers, main="Spectral Analysis of AirPassengers Data")

# Plot the periodogram
plot(spectrum_analysis$freq, spectrum_analysis$spec, type="l",
     xlab="Frequency", ylab="Spectral Density",
     main="Periodogram of AirPassengers Data")




#### Example 2


library(forecast)
library(ggplot2)
library(tseries)

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

# Fourier series decomposition using the Fourier terms
fourier_terms <- fourier(co2_ts, K=2)  # Using K=2 for two harmonics

# Fit a linear model with Fourier terms
model_fourier <- lm(co2_ts ~ fourier_terms)
summary(model_fourier)

# Spectral analysis using periodogram
spectrum_analysis <- spectrum(co2_ts, main="Spectral Analysis of CO2 Data")

# Plot the periodogram
plot(spectrum_analysis$freq, spectrum_analysis$spec, type="l",
     xlab="Frequency", ylab="Spectral Density",
     main="Periodogram of CO2 Data")
