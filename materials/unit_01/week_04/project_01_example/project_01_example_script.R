setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_01/week_04")

library(pacman)
p_load(tidyquant,dplyr,readr,forecast,fpp3,scales)

# Exploratory Data Analysis

if(!file.exists("../inputs/carrot_1990.rds")){
  carrot <- tq_get(c("WPU01130212"),get = "economic.data",from="1980-01-01")
  saveRDS(carrot,"../inputs/carrot_1990.rds")
} else {
  carrot <- readRDS("../inputs/carrot_1990.rds")
}

if(!file.exists("../inputs/carrot_inputs.rds")){
  carrot_inputs <- tq_get(c("WPU01130212","WPU0652"),get = "economic.data",from="1980-01-01")
  saveRDS(carrot_inputs,"../inputs/carrot_inputs.rds")
} else {
  carrot_inputs <- readRDS("../inputs/carrot_inputs.rds")
}

write_csv(carrot,"project_01_example/carrot_prices.csv")
write_csv(carrot_inputs,"project_01_example/fertilizer_prices.csv")


carrot_ts <- carrot %>%
  select(date,price) %>%
  mutate(date=yearmonth(date)) %>%
  as_tsibble()

snames <- tibble(symbol=c("WPU01130212","WPU0652"),
                 name=c("Carrot PPI 1982=100","Fertilizer PPI 1982=100"))

carrot_ts %>%
  model(stl=STL(price)) %>%
  components() %>%
  as_tsibble() %>%
  autoplot(price,color="lightgray") +
  geom_line(aes(y=trend),color="darkorange",size=1) +
  #scale_y_continuous(labels = dollar) +
  labs(y=NULL,x=NULL,title = "Carrot Producer Price Index 1982=100") +
  theme_bw(base_size = 15)

summary(carrot_ts$price)

# Carrot Price Decomposition

carrot_ts %>%
  model(classical_decomposition(price,type="additive")) %>%
  components() %>%
  autoplot() +
  #geom_line(aes(y=trend),color="darkorange",size=1) +
  #scale_y_continuous(labels = dollar) +
  labs(y=NULL,x=NULL,title = "Carrot Producer Price Index 1982=100") +
  theme_bw(base_size = 13)

carrot_inputs %>%
  inner_join(snames) %>%
  ggplot(aes(x=yearmonth(date),y=price)) +
  geom_line() +
  facet_wrap(~name,ncol = 1,scales = "free") +
  labs(y=NULL,x=NULL) +
  theme_bw(base_size = 13)

# Carrot Price Forecast
carrot_ts %>%
  #filter(date>yearmonth("2000 Jan")) %>%
  model(
    additive = ETS(price ~ error("A") + trend("A") + season("A"))
  ) %>%
  forecast(h = "5 years") %>%
  autoplot(carrot_ts %>% filter(date>yearmonth("2000 Jan")),level=80) +
  labs(y=NULL,x=NULL,title = "Carrot Producer Price Index 1982=100") +
  theme_bw(base_size = 15)


# Define the model
fit <- carrot_ts %>%
  model(my_ets = ETS(price ~ trend(method="A") + season(method="A") + error(method="A"))) # Fit model

# Generate forecast
carrot_forecast <- fit %>%
  fabletools::forecast(h = "5 years") # Forecast model for 5 years

# Plot the forecast
autoplot(carrot_forecast, carrot_ts)

# Extract 95% prediction interval
carrot_forecast_pi <- carrot_forecast %>%
  hilo(level = 95) %>% # Extract 95% prediction interval
  unpack_hilo(cols = "95%") # Unpack into own columns named lower and upper


# Prepare data for export
carrot_forecast_out <- carrot_forecast_pi %>%
  select(date, 
         price = .mean, 
         lower = "95%_lower",
         upper = "95%_upper")

to_export <- bind_rows(carrot_ts, carrot_forecast_out)

write_csv(to_export,"project_01_example/carrot_forecast.csv")
