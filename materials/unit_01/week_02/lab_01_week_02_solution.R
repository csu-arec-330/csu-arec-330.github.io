# This is the script for lab 06.

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_01/week_02")

# Comment out the following line if you have already installed the forecast package.
# install.packages("forecast")

# Load necessary libraries
library(tidyquant)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(forecast)

# Part 1: Time Series Decomposition

# Retrieve time series data for carrots
carrot <- tq_get(c("WPU01130212"),
                 get = "economic.data",
                 from = "2007-08-01")

# Convert data into a time series object
carrot_ts <- ts(carrot$price,
                start = c(2007,8),
                frequency = 12)

# Decompose the time series
carrot_decomp <- decompose(carrot_ts)

# Compute seasonally adjusted price
carrot_sa_price <- carrot_ts - carrot_decomp$seasonal

# Convert to a tibble for easier handling
carrot_decomp_sa <- tibble(
  measure_date = carrot$date,
  price = carrot$price,
  trend = carrot_decomp$trend,
  seasonal = carrot_decomp$seasonal,
  residual = carrot_decomp$random,
  sa_price = as.numeric(carrot_sa_price) # Seasonally adjusted price converted to numeric for plotting
) %>%
  drop_na()

# Plot original prices vs. seasonally adjusted prices
sa_plot <- ggplot(carrot_decomp_sa, aes(x = measure_date)) +
  geom_line(aes(y = price, color = "Original Price"), size = 1) +
  geom_line(aes(y = sa_price, color = "Seasonally Adjusted Price"), linewidth = 1, linetype = "dashed") +
  labs(title = "Carrot Prices vs. Seasonally Adjusted Prices",
       x = "Date", y = "Price",
       color = "Legend") +
  theme_minimal()

print(sa_plot)

# Plot decomposed components
plot(carrot_decomp$trend)
plot(carrot_decomp$seasonal)
plot(carrot_decomp$random)

# Store decomposition results into a dataframe
carrot_decomp_out <- carrot_decomp[1:4] %>%
  as_tibble() %>%
  rename(price = x) %>%
  mutate(measure_date = carrot$date,
         forecast = FALSE) %>%
  drop_na()

# Part 2: Forecasting

# Extract trend, seasonal, and residual components
which(is.na(carrot_decomp$trend)) # Determine which values are missing
carrot_trend <- na.omit(carrot_decomp$trend)

which(is.na(carrot_decomp$seasonal))
carrot_seasonal <- na.omit(carrot_decomp$seasonal)[7:(length(carrot_ts) - 6)] %>%
  ts(., start = c(2008, 2), frequency = 12)

which(is.na(carrot_decomp$residuals)) # Determine which values are missing
carrot_residuals <- na.omit(carrot_decomp$random)

# Forecast trend and seasonal components
carrot_trend_forecast <- forecast(carrot_trend, level = 95, h = 60)
plot(carrot_trend_forecast)

carrot_seasonal_forecast <- forecast(carrot_seasonal, level = 95, h = 60)
plot(carrot_seasonal_forecast)

# Fit an MA(1) model for residuals and forecast
carrot_residuals_ts <- ts(carrot_residuals, frequency = 12, start = c(2007, 8))
carrot_residuals_model <- arima(carrot_residuals_ts, order = c(0,0,1))
carrot_residuals_forecast <- forecast(carrot_residuals_model, level = 95, h = 60)

# Combine forecasted components
carrot_forecast <- carrot_trend_forecast$mean + 
  carrot_seasonal_forecast$mean + 
  carrot_residuals_forecast$mean

View(carrot_forecast)

carrot_forecast_upper <- carrot_trend_forecast$upper + 
  carrot_seasonal_forecast$upper + 
  carrot_residuals_forecast$upper

View(carrot_forecast_upper)

carrot_forecast_lower <- carrot_trend_forecast$lower + 
  carrot_seasonal_forecast$lower + 
  carrot_residuals_forecast$lower

View(carrot_forecast_lower)

# Convert forecasted values into a tibble with corresponding dates
max(carrot_decomp_out$measure_date) # Determine the last date of the time series decomposition to determine which was the first forecasted date

carrot_forecast_df <- tibble( # Create a tibble (a modern dataframe in R)
  price = carrot_forecast, # Stores the forecasted price values
  upper = carrot_forecast_upper[,1], # Extracts the upper confidence interval for the forecast
  lower = carrot_forecast_lower[,1] # Extracts the lower confidence interval for the forecast
  ) %>%
  mutate(measure_date = seq(as_date(max(carrot_decomp_out$measure_date)) + months(1), 
                            by = "months", 
                            length.out = nrow(.)))

View(carrot_forecast_df)

# Keep only forecasted data that occurs after the last observed date
carrot_forecast_df <- carrot_forecast_df %>%
  filter(measure_date > max(carrot_decomp_out$measure_date)) %>%
  mutate(forecast = TRUE) # Mark these as forecasted values

# Append forecasted data to the original dataset
final_out <- bind_rows(carrot_decomp_out,carrot_forecast_df)

View(final_out)

# Save the final dataset as a CSV file
write_csv(final_out,"carrot_forecast.csv")