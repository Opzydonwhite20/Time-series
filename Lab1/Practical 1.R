# Load necessary libraries
library(tidyverse)
library(forecast)

# Sales data
sales_data <- c(500, 520, 510, 530, 540, 550, 560, 570, 580, 590, 600, 610,
                620, 630, 640, 650, 660, 670, 680, 690, 700, 710, 720, 730)

# Time series object
sales_ts <- ts(sales_data, start = c(2022, 1), frequency = 12)

# 1. Calculate 3-month moving average
ma_sales <- ma(sales_ts, order = 3)
ma_sales


# 2. Fit a linear trend using least squares method
linear_model <- lm(sales_data ~ time(sales_ts))
linear_model

# Create time points for the forecast
future_time_points <- data.frame(time_sales_ts = c(25, 26, 27))

# 3. Forecast sales for the next 3 months using both methods
ma_forecast <- forecast(ma_sales, h = 3)
ma_forecast

linear_forecast <- predict(linear_model, newdata = future_time_points)
linear_forecast

# Print results
list(ma_forecast = ma_forecast, linear_forecast = linear_forecast)


#####=========PROBLEM 2

# Load necessary libraries
library(tidyverse)
library(forecast)

# Temperature data
temp_data <- c(2.1, 3.0, 5.5, 9.2, 14.0, 18.2, 21.0, 20.5, 16.5, 10.0, 5.0, 2.5,
               2.3, 3.2, 5.7, 9.5, 14.3, 18.5, 21.3, 20.8, 16.8, 10.2, 5.3, 2.8,
               2.5, 3.4, 6.0, 9.8, 14.6, 18.7, 21.5, 21.0, 17.0, 10.5, 5.5, 3.0)

# Time series object
temp_ts <- ts(temp_data, start = c(2021, 1), frequency = 12)

# 1. Calculate 6-month moving average
ma_temp <- ma(temp_ts, order = 6)

# 2. Fit a linear trend using least squares method
linear_model_temp <- lm(temp_data ~ time(temp_ts))

# Create time points for the forecast
future_time_points_temp <- data.frame(time_temp_ts = c(37, 38, 39))

# 3. Forecast temperatures for the next 3 months using both methods
ma_forecast_temp <- forecast(ma_temp, h = 3)
linear_forecast_temp <- predict(linear_model_temp, newdata = future_time_points_temp)

# Print results
list(ma_forecast_temp = ma_forecast_temp, linear_forecast_temp = linear_forecast_temp)