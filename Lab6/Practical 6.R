# Load the AirPassengers dataset
data("AirPassengers")


data <-  AirPassengers
data

# Plot the original time series data
plot(AirPassengers, main = "Monthly Airline Passenger Numbers (1949-1960)",
     ylab = "Passengers", xlab = "Year")

# Compute the periodogram
airpassengers_periodogram <- spec.pgram(AirPassengers, log = "no", plot = TRUE)

# Plot the periodogram
plot(airpassengers_periodogram, main = "Periodogram of Monthly Airline Passenger Numbers",
     xlab = "Frequency", ylab = "Spectral Density",
     col = "blue", type = "h")





## Example 2

# Load necessary libraries
library(ggplot2)

# Load the Nile dataset
data("Nile")

# Convert the dataset to a data frame
nile_df <- data.frame(
  Year = time(Nile),
  Flow = as.numeric(Nile)
)

# Plot the Nile time series
ggplot(nile_df, aes(x = Year, y = Flow)) +
  geom_line() +
  labs(title = "Annual Flow of the Nile River", x = "Year", y = "Flow") +
  theme_minimal()

# Compute and plot the periodogram
spec.pgram(Nile, main = "Periodogram of Annual Flow of the Nile River")



# Plot the periodogram
plot(Nile, main = "Periodogram of Monthly Nile River",
     xlab = "Frequency", ylab = "Spectral Density",
     col = "blue", type = "h")
