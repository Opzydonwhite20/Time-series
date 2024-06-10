# Load necessary libraries
library(forecast)
library(ggplot2)



## Problem 1

years <- 2001:2023
sales <- c(2130, 7582, 2960, 9347, 3943, 7338, 7704, 3231, 7993, 3017, 4303, 3460, 2126, 8298, 7257, 5848, 8320, 7735, 4010, 9977, 6934, 3536, 2773)

# Create a time series object
sales_ts <- ts(sales, start = 2001, frequency = 1)

# Plot the sales data
plot(sales_ts, main = "Annual Sales Data", ylab = "Sales", xlab = "Year")


# Fit AR(1) model
ar1_model <- arima(sales_ts, order = c(1, 0, 0))
summary(ar1_model)

# Fit AR(2) model
ar2_model <- arima(sales_ts, order = c(2, 0, 0))
summary(ar2_model)


# Forecast future values using the AR(1) model
forecast_ar1 <- forecast(ar1_model, h=12)
plot(forecast_ar1)
title(main="12-Month Temperature Forecast using AR(1) Model")

# Forecast future values using the AR(2) model
forecast_ar2 <- forecast(ar2_model, h=12)
plot(forecast_ar2)
title(main="12-Month Temperature Forecast using AR(2) Model")


# Compare the models using AIC
ar1_aic <- AIC(ar1_model)
ar2_aic <- AIC(ar2_model)

cat("AIC for AR(1) model: ", ar1_aic, "\n")
cat("AIC for AR(2) model: ", ar2_aic, "\n")

# Choose the model with the lower AIC
best_model <- ifelse(ar1_aic < ar2_aic, "AR(1)", "AR(2)")
cat("The best model is: ", best_model, "\n")






#### PROBLEM 2

temperature <- c(20.67961519, 20.65293922, 20.18923375, 20.86448871, 
       20.67913765, 20.99650512, 21.47251432, 20.89406671, 
       23.16363374, 24.21791674)

# Plot the simulated data
data <- data.frame(time=time, temperature=temperature)
ggplot(data, aes(x=time, y=temperature)) +
  geom_line() +
  labs(title="Simulated Monthly Average Temperature", x="Month", y="Temperature")

# Fit AR(1) model
ar1_model <- arima(temperature, order=c(1,0,0))
summary(ar1_model)

# Fit AR(2) model
ar2_model <- arima(temperature, order=c(2,0,0))
summary(ar2_model)

# Forecast future values using the AR(1) model
forecast_ar1 <- forecast(ar1_model, h=12)
plot(forecast_ar1)
title(main="12-Month Temperature Forecast using AR(1) Model")

# Forecast future values using the AR(2) model
forecast_ar2 <- forecast(ar2_model, h=12)
plot(forecast_ar2)
title(main="12-Month Temperature Forecast using AR(2) Model")

ar1_aic <- AIC(ar1_model)
ar2_aic <- AIC(ar2_model)

cat("AIC for AR(1) model: ", ar1_aic, "\n")
cat("AIC for AR(2) model: ", ar2_aic, "\n")

# Choose the model with the lower AIC
best_model <- ifelse(ar1_aic < ar2_aic, "AR(1)", "AR(2)")
cat("The best model is: ", best_model, "\n")


