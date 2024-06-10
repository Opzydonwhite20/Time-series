library(forecast)
library(ggplot2)
library(car)

time <- c(1:10)
temperature <- c(20.67961519, 20.65293922, 20.18923375, 20.86448871, 
                 20.67913765, 20.99650512, 21.47251432, 20.89406671, 
                 23.16363374, 24.21791674)

# Plot the simulated data
ggplot(data, aes(x=time, y=temperature)) +
  geom_line() +
  labs(title="Simulated Monthly Average Temperature", x="Month", y="Temperature")

# Fit a linear regression model
model <- lm(temperature ~ time, data=data)

# Perform Durbin-Watson test
dw_test <- durbinWatsonTest(model)
print(dw_test)



### EXample 2

library(lmtest)

# Perform Breusch-Godfrey test for serial correlation up to lag 1
bg_test <- bgtest(model, order=1)
print(bg_test)

# Perform Breusch-Godfrey test for serial correlation up to lag 2
bg_test2 <- bgtest(model, order=2)
print(bg_test2)


