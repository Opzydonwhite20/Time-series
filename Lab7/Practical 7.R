# Load the lynx dataset
data("lynx")

data = lynx
data

# Plot the original time series data
plot(lynx, main = "Annual Number of Lynx Trapped (1821-1934)",
     ylab = "Number of Lynx", xlab = "Year")


# Compute the periodogram
lynx_periodogram <- spec.pgram(lynx, log = "no", plot = FALSE)

# Plot the periodogram
plot(lynx_periodogram, main = "Periodogram of Annual Lynx Trapping Data",
     xlab = "Frequency", ylab = "Spectral Density",
     col = "blue", type = "h")


# Load necessary library
library(stats)

# Fit an AR model to the lynx data
lynx_ar_model <- ar(lynx)
lynx_ar_model

# Print the AR model summary
summary(lynx_ar_model)


# Predict using the AR model
lynx_fitted_values <- predict(lynx_ar_model, n.ahead = length(lynx))$pred

# Plot original data and fitted values
plot(lynx, main = "AR Model Fitting to Annual Lynx Trapping Data",
     ylab = "Number of Lynx", xlab = "Year", col = "black")
lines(lynx_fitted_values, col = "red")
legend("topright", legend = c("Original Data", "Fitted Values"),
       col = c("black", "red"), lty = 1)





### Example 2 


# Load the necessary libraries
library(forecast)
library(stats)

# Load the AirPassengers dataset
data("AirPassengers")

# Plot the original time series data
plot(AirPassengers, main = "Monthly Airline Passenger Numbers (1949-1960)",
     ylab = "Passengers", xlab = "Year")

# Fit an AR model to the data
# Use auto.arima to select the best ARIMA model, setting d=0 to ensure it's an AR model
ar_model <- auto.arima(AirPassengers, d = 0, seasonal = FALSE)

# Print the summary of the AR model
summary(ar_model)

# Extract the residuals of the AR model
residuals_ar <- residuals(ar_model)

# Plot the residuals
plot(residuals_ar, main = "Residuals of the AR Model",
     ylab = "Residuals", xlab = "Time")

# Compute the periodogram of the residuals
periodogram_residuals <- spec.pgram(residuals_ar, log = "no", plot = FALSE)

# Plot the periodogram of the residuals
plot(periodogram_residuals, main = "Periodogram of the Residuals of the AR Model",
     xlab = "Frequency", ylab = "Spectral Density",
     col = "blue", type = "h")

