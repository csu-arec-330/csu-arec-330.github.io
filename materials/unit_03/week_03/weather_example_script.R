
# This is the script used to generate the slides for lecture_03_week_03.qmd

library(pacman)
p_load(tidyverse,modelsummary,sf,progress,janitor,tigris,mapview,tidycensus,dplyr,GGally,scales)

# =============== Option 2 =============== #
# ------- Working with Weather Data ------ #
# ======================================== #

# Research Question: 
# What is the relationship between **total monthly rainfall** and **total monthly sales** at the county level?

# Part 1: Read in raw data
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/shopper_info.csv.gz")

# Read in the weather data
weather_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/gridmet_county_daily_2023-2023.csv.gz")

# Read in convenience store location dataset
store_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/store_info.csv.gz")

# Plot store coordinates and convert to spatial data frame
store_geo <- store_raw %>%
  
  # First, we subset the variables `store_id` and the coordinates.
  select(store_id, latitude, longitude) %>% 
  
  # Tells R that these are essentially $x$ and $y$ coordinates in a certain projection (represented by the code 4326).
  st_as_sf(coords=c("longitude","latitude"), crs = st_crs(4326))  

length(store_geo$store_id)

# Fetch data for county polygons using the 'tigris' package
us_co_filtered <- tigris::counties(cb=T,class="sf") %>%
  janitor::clean_names() %>%
  mutate(aland=aland/2.59e+6) %>%
  st_transform(4326) %>%
  filter(!statefp %in% c("60", "66", "72", "02", "15", "69", "78")) # Keep only counties in contiguous US

length(us_co_filtered$geoid)

plot(st_geometry(us_co_filtered))
plot(st_geometry(store_geo), add = TRUE, col = "red")

# Join county to store_geo
store_co_geo <- st_join(store_geo, us_co_filtered, join=st_intersects)

length(unique(store_co_geo$geoid))

# Aggregate daily sales by store from transaction-level data
store_sales <- shopper_info %>%
  mutate(measure_date = as_date(date_time)) %>%              # Extract date component from timestamp
  group_by(store_id, measure_date) %>%                       # Group by store and date
  summarize(
    sales = sum(unit_price * unit_quantity, na.rm = TRUE),   # Compute total sales (price × quantity)
    .groups = 'drop'
  )

# Reshape weather data from long to wide format (each variable becomes a column)
weather_wide <- weather_raw %>%
  pivot_wider(
    id_cols = c(county, date),              # Keep county and date as identifiers
    names_from = variable,                  # Each unique variable becomes a new column
    values_from = value                     # Fill those columns with the corresponding values
  ) %>%
  mutate(statefp = substring(county,1,2)) %>%
  arrange(county)

# Join daily weather data to full county data using date and county identifiers
us_co_weather <- us_co_filtered %>%
  st_set_geometry(NULL) %>%
  select(geoid, state_name, namelsad) %>%
  right_join(weather_wide, 
             by = c("geoid" = "county"))

length(unique(us_co_weather$geoid)) # N=3,104

# Add county GEOID to the store-level sales data (so we can link to county-level weather data)
store_sales_co <- store_co_geo %>%
  st_set_geometry(NULL) %>%
  select(store_id, geoid) %>%                # Keep only store_id (for joining) and geoid (for county-level linkage)
  inner_join(store_sales, by = "store_id")   # Join to sales data using store_id; keep only stores with known county info

store_sales_weather_co <- us_co_weather %>%
  left_join(store_sales_co, 
            by = c("date" = "measure_date", 
                   "geoid" = "geoid")) %>%
  filter(month(date)==7) %>%
  arrange(geoid, date, store_id)

length(unique(store_sales_weather_co$geoid)) # N=3,104

write_csv(store_sales_weather_co, "C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_03/inputs/store_sales_weather_co.csv")

# ======================================== #
# ------ Monthly ------ #
# ======================================== #

# Aggregate store sales and weather data to the month
sales_weather_month <- store_sales_weather_co %>%
  mutate(month = month(date),
         year = year(date),
         day = day(date),
         ones = if_else(is.na(store_id), 1, 0)) %>%
  group_by(geoid, state_name, namelsad, statefp, month) %>%
  summarize(
    total_rainfall = sum(pr_precipitation_amount, na.rm = TRUE),
    avg_humidity = mean(rmax_relative_humidity, na.rm = TRUE),
    avg_temp = mean(tmmx_air_temperature, na.rm = TRUE),
    total_sales = sum(sales, na.rm = TRUE),
    store_count = sum(ones, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(sales_per_store = total_sales / store_count)

length(unique(sales_weather_month$geoid)) # N=3,104

write_csv(sales_weather_month, "C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_03/inputs/sales_weather_month.csv")

# Part 2: Plot the Data

datasummary_skim()

# Basic scatter plot with ggplot
sales_weather_month %>%
  select(total_rainfall, avg_temp, store_count, total_sales) %>%
  ggpairs()

# Zoom in on scatter plot
sales_weather_month_slim <- sales_weather_month %>%
  select(total_rainfall, avg_temp, store_count, total_sales, month)

# Basic scatter plot with ggplot
scatter_plot <- ggplot(sales_weather_month_slim, 
                       aes(x = total_rainfall, y = total_sales)) + 
  geom_point(alpha=.6) +  
  labs(subtitle = "Relationship between Total Rainfall and Total Sales",
       x = "Total Inches of Rain",
       y = "Total Sales (Dollars)") +
  theme_bw(base_size = 15)

scatter_plot # Print scatter plot

# Advanced scatter plot with ggplot (additional formatting)
scatter_plot <- ggplot(sales_weather_month_slim, 
                       aes(x = total_rainfall, y = total_sales)) + 
  geom_point(alpha=.6) +  
  labs(subtitle = "Relationship between Total Rainfall and Total Sales",
       x = "Total Inches of Rain",
       y = "Total Sales (Dollars)") +
  theme_bw(base_size = 15) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +  # <- x-axis line at y = 0
  theme(
    panel.border = element_blank(),                  # Remove border
    axis.line.y = element_line(color = "black"),     # Only y-axis line
    axis.line.x = element_blank()                    # Remove x-axis line at y = 0
  ) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))  # e.g., 100,000 → 100K

scatter_plot # Print scatter plot

# Part 4: Estimate Regression - univariate
model1 <- lm(total_sales ~ total_rainfall, sales_weather_month_slim) # Modeling the relationship between total rainfall and store sales at the county level. Store it as "model1"

uni_out <- modelsummary(model1,
                        fmt = 5, # Format estimate so it shows five decimal places
                        stars = TRUE,
                        coef_rename = c("(Intercept)"="Intercept",
                                        "total_rainfall"="Total Rainfall"),
                        gof_map = c("nobs", "r.squared"))

uni_out # Print regression output

# Create new data for predictions
pred_data1 <- sales_weather_month_slim %>%
  dplyr::select(total_rainfall) %>%
  distinct() %>%
  arrange(total_rainfall) %>%
  mutate(fitted = predict(model1, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = total_rainfall, y = fitted), color = "blue", linewidth = 1)


# Estimate regression - multivariate
model2 <- lm(total_sales ~ total_rainfall + avg_temp, sales_weather_month_slim) # Modeling the relationship between total rainfall, average temperature, and total sales at the county level. Store it as "model2"

multi_out <- modelsummary(model2,
                          fmt = 5, # Format estimate so it shows five decimal places
                          stars = TRUE,
                          coef_rename = c("(Intercept)"="Intercept",
                                          "total_rainfall"="Total Rainfall",
                                          "avg_temp" = "Average Temperature"),
                          gof_map = c("nobs", "r.squared"))

multi_out # Print regression output

# Create new data for predictions
pred_data2 <- sales_weather_month_slim %>%
  dplyr::select(total_rainfall, avg_temp) %>%
  distinct() %>%
  arrange(total_rainfall, avg_temp) %>%
  mutate(fitted = predict(model2, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = total_rainfall, y = fitted), color = "blue", linewidth = 1) +
  geom_line(data = pred_data2, aes(x = total_rainfall, y = fitted), color = "red", linewidth = 1)


# Estimate regression - multivariate
model3 <- lm(total_sales ~ total_rainfall + avg_temp + store_count, sales_weather_month_slim) # Modeling the relationship between total rainfall, average temperature, and total sales at the county level. Store it as "model2"

multi2_out <- modelsummary(model3,
                             fmt = 5, # Format estimate so it shows five decimal places
                             stars = TRUE,
                             coef_rename = c("(Intercept)"="Intercept",
                                             "total_rainfall"="Total Rainfall",
                                             "avg_temp" = "Average Temperature",
                                             "store_count" = "Number of Stores"),
                             gof_map = c("nobs", "r.squared"))

multi2_out # Print regression output

# Create new data for predictions
pred_data3 <- sales_weather_month_slim %>%
  dplyr::select(total_rainfall, avg_temp, store_count) %>%
  distinct() %>%
  arrange(total_rainfall, avg_temp, store_count) %>%
  mutate(fitted = predict(model3, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = total_rainfall, y = fitted), color = "blue", linewidth = 1) +
  geom_line(data = pred_data2, aes(x = total_rainfall, y = fitted), color = "red", linewidth = 1) +
  geom_line(data = pred_data3, aes(x = total_rainfall, y = fitted), color = "yellow", linewidth = 1)
  
