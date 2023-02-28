#This script reads in and processes data from FRED

library(readr)
library(dplyr)
library(lubridate)

###
egg_raw <- read_csv("materials/unit_01/inputs/APU0000708111.csv")

#Change format of date
egg_raw %>%
  mutate(CHAR_DATE=format(DATE,"%m/%d/%Y")) %>%
  select(-DATE) %>%
  write_csv("materials/unit_01/inputs/APU0000708111_cdate.csv")


egg_raw_c <- read_csv("https://csu-arec-330.github.io/materials/unit_01/inputs/APU0000708111_cdate.csv")


library(tidyquant)


tq_get(c("APU0000708111"),get = "economic.data",from="1990-01-01")

#WPU017107: Producer Price Index by Commodity: Farm Products: Eggs for Fresh Use
#WPU012: Producer Price Index by Commodity: Farm Products: Grains
#WPU023: Producer Price Index by Commodity: Processed Foods and Feeds: Dairy Products 
fred_data <- tq_get(c("WPU017107","WPU012","WPU023"),get = "economic.data",from="1900-01-01")


#######################################
#Lab 02

#Tutorial here: https://kevin-kotze.gitlab.io/tsm/ts-2-tut/
install.packages("changepoint")
library(changepoint)
install.packages("strucchange")
library(strucchange)
set.seed(123) 

sim_mean <- c(rnorm(180, 0, 1),
              rnorm(180, 1.5, 1))

sim_mean <- rnorm(360, 0, 1)
              
plot.ts(sim_mean)

#m_binseg <- cpt.mean(sim_mean, penalty = "BIC", method = "BinSeg", Q = 5)

#plot(m_binseg, type = "l", xlab = "Index", cpt.width = 4)

reg_dat <- tibble(y=sim_mean) %>%
  mutate(x=row_number())
                  
test_lm <- lm(y ~ x,data=reg_dat)
summary(test_lm)


################################
#Week 2
#Read in Ali's carrots data
carrot <- tq_get(c("WPU01130212"),get = "economic.data",from="2007-08-01")

carrot_ts <- ts(carrot$price,frequency = 12,start=c(2007,8))

carrot_decomp <- decompose(carrot_ts)

plot(carrot_decomp$trend)


plot(carrot_decomp)

carrot_decomp_out <- carrot_decomp[1:4] %>%
  as_tibble() %>%
  rename(price=x) %>%
  mutate(measure_date=carrot$date)

write_csv(carrot_decomp_out,"carrot_decomp.csv")

# Extract the trend, seasonal, and residual components
carrot_trend <- carrot_decomp$trend
carrot_seasonal <- carrot_decomp$seasonal
carrot_residuals <- carrot_decomp$random

library(forecast)

# Forecast the trend component for the next 12 months
carrot_trend_forecast <- forecast(carrot_trend, h=60)

plot(carrot_trend_forecast)

# Forecast the seasonal component for the next 12 months
carrot_seasonal_forecast <- forecast(carrot_seasonal, h=60)

plot(carrot_seasonal_forecast)

# Create a time series object for the residuals
carrot_residuals_ts <- ts(carrot_residuals, frequency=12, start=c(2007, 8))

# Fit an MA(1) model to the residuals
carrot_residuals_model <- arima(carrot_residuals_ts, order=c(0,0,1))

# Forecast the residuals for the next 12 months
carrot_residuals_forecast <- forecast(carrot_residuals_model, h=60)$mean

# Combine the forecasted components to obtain the final forecast
carrot_forecast <- carrot_trend_forecast$mean + carrot_seasonal_forecast$mean + carrot_residuals_forecast

# Print the forecast for the next 12 months
plot(carrot_forecast)

######################################################
#Week 3
