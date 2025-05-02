
# These are the lab notes for Week 14

library(pacman)
p_load(tidyverse,modelsummary,sf,progress,janitor,tigris,mapview,tidycensus,dplyr,GGally,scales)

setwd("Set your working directory")
getwd() # Confirm you are in the correct working directory

# =============== Option 1=============== #
# ------- Working with Census Data ------ #
# ======================================= #

# Research Question: 
# What is the relationship between **median income** and **total store count** at the county level?

# Part 1: Read in Spatial Data

# Read in convenience store location dataset
store_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/store_info.csv.gz")

# Read in the shopper sales dataset
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/shopper_info.csv.gz")

# Convert data frame to a points simple feature object
store_geo <- store_raw %>%
  
  # First, we subset the variables `store_id` and the coordinates.
  select(store_id, latitude, longitude) %>% 
  
  # Tells R that these are essentially $x$ and $y$ coordinates in a certain projection (represented by the code 4326). 
  st_as_sf(coords=c("longitude","latitude"), crs = st_crs(4326))  

# Fetch data for county polygons using the 'tigris' package
us_co <- tigris::counties(cb=T,class="sf") %>%
  janitor::clean_names() %>%
  mutate(aland=aland/2.59e+6) %>%
  st_transform(4326)

# Look at the distinct state list by statefp code
unique_statefp <- us_co %>%
  st_set_geometry(NULL) %>%  # Remove geometry column
  select(stusps, statefp) %>%  # Select the columns of interest
  distinct() %>%  # Remove duplicates, keeping only unique rows
  arrange(stusps)  # Arrange alphabetically by stusps

# Filter out counties from American Samoa (60), Guam (66), Saipan Municipality (69), Puerto Rico (72), Virgin Islands (78), Alaska (02), and Hawaii (15)
us_co_filtered <- us_co %>%
  filter(!statefp %in% c("60", "66", "72", "02", "15", "69", "78"))

# Join county to store_geo
store_co_geo <- st_join(store_geo, 
                        us_co_filtered, 
                        join=st_intersects)

# Aggregate store count by county
store_count_by_county <- store_co_geo %>%
  group_by(geoid) %>%
  summarize(
    store_count = n(), # Count the number of stores in each county
    .groups = 'drop')  # Drop groups to prevent regrouping

# Join aggregated data back with county geometries for mapping
county_store_map <- st_join(us_co_filtered, 
                            store_count_by_county, 
                            join=st_intersects)

# Create store sales by county
sales_by_county <- shopper_info %>%
  select(store_id, unit_price, unit_quantity) %>%
  mutate(sales = unit_price * unit_quantity) %>%
  group_by(store_id) %>%
  summarize(
    total_sales = sum(sales),
    .groups = 'drop'
  ) %>%
  left_join(store_co_geo, by = "store_id") %>%
  group_by(geoid) %>%
  summarize(
    county_sales = sum(total_sales),
    .groups = 'drop'
  )

# Part 2: Bring in Census Data

# Load variable metadata for the 2022 ACS 5-year estimates (used to look up variable codes and labels)
census_22 <- load_variables(2022, "acs5", cache = TRUE)

# --- Household income --- #
# Download median household income data from the 2022 ACS 5-year estimates at the county level
census_hhi <- get_acs(
  geography = "county",                      # Get data for all U.S. counties
  survey = "acs5",                           # Use 5-year ACS data (more reliable for small areas)
  variables = c(medincome = "B19013_001"),   # B19013_001 = median household income
  state = NULL,                              # NULL = include all states (not just one)
  year = 2022                                # Use the most recent available year
)

# Clean column names and keep only GEOID and the income estimate, renamed as 'hhi'
hhi <- census_hhi %>%
  clean_names() %>%
  select(geoid, hhi = estimate)

# --- Population --- #
# Download population data from the 2022 ACS 5-year estimates at the county level
census_pop <- get_acs(
  geography = "county",                      # Get data for all U.S. counties
  survey = "acs5",                           # Use 5-year ACS data (more reliable for small areas)
  variables = c(pop = "B01003_001"),         # B01003_001 = total population
  state = NULL,                              # NULL = include all states (not just one)
  year = 2022                                # Use the most recent available year
)

# Clean column names and keep only GEOID and the population estimate, renamed as 'pop'
pop <- census_pop %>%
  clean_names() %>%
  select(geoid, pop = estimate)

# Join median household income (hhi) to the store-level dataset using county GEOID
demo <- hhi %>%
  left_join(pop, by = "geoid")

# Join median household income (hhi) and total population (pop) to the county-store map using county GEOID
county_demo <- county_store_map %>%
  select(-geoid.y) %>%
  rename(geoid = geoid.x) %>%
  inner_join(demo, by = "geoid") %>%   # Join on county identifier (GEOID)
  mutate(popden = pop/aland) %>% # Calculate population density (population over land in square miles)
  left_join(sales_by_county, by = "geoid") %>%
  st_set_geometry(NULL)          # Drop geometry to simplify data manipulation
  
length(county_demo$geoid)

write_csv(county_demo, "C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_03/inputs/county_demo.csv")


# Part 3: Plot the Data

# Data summary
datasummary_skim(county_demo, type = "numeric")
datasummary_skim(county_demo, type = "categorical")

# EDA with ggpairs
county_demo %>%
  mutate(store_count = replace_na(store_count, 0)) %>%
  filter(!is.na(hhi)) %>% # Drop counties with missing household income 
  select(hhi, popden, store_count) %>%
  ggpairs()

# Zoom in on scatter plot
county_demo_slim <- county_demo %>%
  mutate(store_count = replace_na(store_count, 0)) %>%
  filter(!is.na(hhi)) %>% # Drop counties with missing household income
  select(hhi, popden, store_count)

# Basic scatter plot with ggplot
scatter_plot <- ggplot(county_demo_slim, 
                       aes(x = hhi, y = store_count)) + 
  geom_point(alpha=.6) +  
  labs(subtitle = "Relationship between Median Household Income and Number of Stores",
       x = "Median Household Income (2022)",
       y = "Store Count (County Level)") +
  theme_bw(base_size = 15)

scatter_plot # Print scatter plot

# Advanced scatter plot with ggplot (additional formatting)
scatter_plot <- ggplot(county_demo_slim, 
                       aes(x = hhi, y = store_count)) + 
  geom_point(alpha=.6) +  # Add points for scatter plot
  labs(subtitle = "Relationship between Median Household Income and Number of Stores",
       x = "Median Household Income (2022)",
       y = "Store Count (County Level)") +
  theme_bw(base_size = 15) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +  # <- x-axis line at y = 0
  theme(
    axis.line = element_line(color = "black"),   # Draw axis lines
    panel.border = element_blank()               # Remove default plot border
  ) + 
  scale_x_continuous(
    breaks = seq.int(0, 10, 2),
    expand = c(0, 0)
  )

scatter_plot # Print scatter plot

# Part 4: Estimate Regression - univariate
model1 <- lm(store_count ~ hhi, county_demo_slim) # Modeling the relationship between household income and store count at the county level. Store it as "model1"

uni_out <- modelsummary(model1,
                      fmt = 5, # Format estimate so it shows five decimal places
                      stars = TRUE,
                      coef_rename = c("(Intercept)"="Intercept",
                                      "hhi"="Median Inc"),
                      gof_map = c("nobs", "r.squared"))

uni_out # Print regression output

# Create new data for predictions
pred_data1 <- county_demo_slim %>%
  dplyr::select(hhi) %>%
  distinct() %>%
  arrange(hhi) %>%
  mutate(fitted = predict(model1, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = hhi, y = fitted), color = "blue", linewidth = 1)

# Estimate regression - multivariate
model2 <- lm(store_count ~ hhi + popden, county_demo_slim) # Modeling the relationship between household income, pop density, and store count at the county level. Store it as "model2"

multi_out <- modelsummary(model2,
                      fmt = 5, # Format estimate so it shows five decimal places
                      stars = TRUE,
                      coef_rename = c("(Intercept)"="Intercept",
                                      "hhi"="Median Inc",
                                      "popden" = "Population Density"),
                      gof_map = c("nobs", "r.squared"))

multi_out # Print regression output

# Create new data for predictions
pred_data2 <- county_demo_slim %>%
  dplyr::select(hhi, popden) %>%
  distinct() %>%
  arrange(hhi) %>%
  mutate(fitted = predict(model2, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = hhi, y = fitted), color = "blue", linewidth = 1) +
  geom_line(data = pred_data2, aes(x = hhi, y = fitted), color = "red", linewidth = 1)


# =============== Option 2 =============== #
# ------- Working with Weather Data ------ #
# ======================================== #

# Research Question: 
# What is the relationship between **total monthly rainfall** and **total montly sales** at the county level?

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

# Fetch data for county polygons using the 'tigris' package
us_co_filtered <- tigris::counties(cb=T,class="sf") %>%
  janitor::clean_names() %>%
  mutate(aland=aland/2.59e+6) %>%
  st_transform(4326) %>%
  filter(!statefp %in% c("60", "66", "72", "02", "15", "69", "78")) # Keep only counties in contiguous US

# Join county to store_geo
store_co_geo <- st_join(store_geo, us_co_filtered, join=st_intersects)

# Aggregate daily sales by store from transaction-level data
store_sales <- shopper_info %>%
  mutate(measure_date = as_date(date_time)) %>%              # Extract date component from timestamp
  group_by(store_id, measure_date) %>%                       # Group by store and date
  summarize(
    sales = sum(unit_price * unit_quantity, na.rm = TRUE)   # Compute total sales (price × quantity)
  ) %>%
  ungroup()                                                  # Remove grouping to clean up the result

# Add county GEOID to the store-level sales data (so we can link to county-level data like weather)
store_sales_co <- store_co_geo %>%
  st_set_geometry(NULL) %>%
  select(store_id, geoid) %>%                # Keep only store_id (for joining) and geoid (for county-level linkage)
  inner_join(store_sales, by = "store_id")   # Join to sales data using store_id; keep only stores with known county info

# Reshape weather data from long to wide format (each variable becomes a column)
weather_wide <- weather_raw %>%
  pivot_wider(
    id_cols = c(county, date),              # Keep county and date as identifiers
    names_from = variable,                  # Each unique variable becomes a new column
    values_from = value                     # Fill those columns with the corresponding values
  )

# Join daily store sales to weather data using date and county identifiers
store_sales_weather <- store_sales_co %>%
  inner_join(weather_wide, 
             by = c("measure_date" = "date", 
                    "geoid" = "county"))

length(unique(store_sales_weather$geoid))

write_csv(store_sales_weather, "C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_03/inputs/store_sales_weather.csv")

# ======================================== #
# ------ Monthly ------ #
# ======================================== #

# Aggregate store sales and weather data to the month
sales_weather_month <- store_sales_weather %>%
  mutate(month = month(measure_date)) %>%
  mutate(year = year(measure_date)) %>%
  mutate(day = day(measure_date)) %>%
  group_by(geoid, month) %>%
  summarize(
    total_rainfall = sum(pr_precipitation_amount),
    avg_humidity = mean(rmax_relative_humidity),
    avg_temp = mean(tmmx_air_temperature),
    total_sales = sum(sales, na.rm = TRUE),
    .groups = 'drop'
  ) %>% # Match on date and county FIPS
  full_join(us_co_filtered %>% 
              st_set_geometry(NULL) %>%    # Remove the spatial element
              select(geoid), 
            by = "geoid")  # Join in the county level fips codes

length(unique(sales_weather_month$geoid))

write_csv(sales_weather_month, "sales_weather_month.csv")

# Part 2: Plot the Data

# Basic scatter plot with ggplot
sales_weather_month %>%
  select(total_rainfall, avg_temp, total_sales) %>%
  ggpairs()

# Zoom in on scatter plot
sales_weather_month_slim <- sales_weather_month %>%
  select(total_rainfall, avg_temp, total_sales, month)

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



# ======================================== #
# ------ Daily ------ # 
# ======================================== #
sales_weather_day <- store_sales_weather %>%
  mutate(month = month(measure_date)) %>%
  mutate(year = year(measure_date)) %>%
  mutate(day = day(measure_date)) %>%
  group_by(geoid, day) %>%
  summarize(
    total_rainfall = sum(pr_precipitation_amount),
    avg_humidity = mean(rmax_relative_humidity),
    avg_temp = mean(tmmx_air_temperature),
    total_sales = sum(sales),
    .groups = 'drop'
  ) %>% # Match on date and county FIPS
  full_join(us_co_filtered %>% 
              st_set_geometry(NULL) %>%    # Remove the spatial element
              select(geoid), 
            by = "geoid")  # Join in the county level fips codes

# Part 2: Plot the Data

# Basic scatter plot with ggplot
sales_weather_day %>%
  select(total_rainfall, avg_temp, total_sales) %>%
  ggpairs()

# Zoom in on scatter plot
sales_weather_day_slim <- sales_weather_day %>%
  select(total_rainfall, avg_temp, total_sales, day)

# Basic scatter plot with ggplot
scatter_plot <- ggplot(sales_weather_day_slim, 
                       aes(x = total_rainfall, y = total_sales)) + 
  geom_point(alpha=.6) +  
  labs(subtitle = "Relationship between Total Rainfall and Total Sales",
       x = "Total Inches of Rain",
       y = "Total Sales (Dollars)") +
  theme_bw(base_size = 15)

scatter_plot # Print scatter plot

# Advanced scatter plot with ggplot (additional formatting)
scatter_plot <- ggplot(sales_weather_day_slim, 
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
model1 <- lm(total_sales ~ total_rainfall, sales_weather_day_slim) # Modeling the relationship between total rainfall and store sales at the county level. Store it as "model1"

uni_out <- modelsummary(model1,
                        fmt = 5, # Format estimate so it shows five decimal places
                        stars = TRUE,
                        coef_rename = c("(Intercept)"="Intercept",
                                        "total_rainfall"="Total Rainfall"),
                        gof_map = c("nobs", "r.squared"))

uni_out # Print regression output

# Create new data for predictions
pred_data1 <- sales_weather_day_slim %>%
  dplyr::select(total_rainfall) %>%
  distinct() %>%
  arrange(total_rainfall) %>%
  mutate(fitted = predict(model1, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = total_rainfall, y = fitted), color = "blue", linewidth = 1)

# What if we control for day?
model1_fe <- lm(total_sales ~ total_rainfall + factor(day), sales_weather_day_slim) # Modeling the relationship between total rainfall and store sales at the county level. Store it as "model1"

fe_out <- modelsummary(model1_fe,
                        fmt = 5, # Format estimate so it shows five decimal places
                        stars = TRUE,
                        coef_rename = c("(Intercept)"="Intercept",
                                        "total_rainfall"="Total Rainfall"),
                        gof_map = c("nobs", "r.squared"))

fe_out # Print regression output


# Estimate regression - multivariate
model2_fe <- lm(total_sales ~ total_rainfall + avg_temp + factor(day), sales_weather_day_slim) # Modeling the relationship between total rainfall, average temperature, and total sales at the county level. Store it as "model2"

multi_out_fe <- modelsummary(model2_fe,
                             fmt = 5, # Format estimate so it shows five decimal places
                             stars = TRUE,
                             coef_rename = c("(Intercept)"="Intercept",
                                             "total_rainfall"="Total Rainfall",
                                             "avg_temp" = "Average Temperature"),
                             gof_map = c("nobs", "r.squared"))

multi_out_fe # Print regression output

# Create new data for predictions
pred_data2 <- sales_weather_day_slim %>%
  dplyr::select(total_rainfall, avg_temp) %>%
  distinct() %>%
  arrange(total_rainfall, avg_temp) %>%
  mutate(fitted = predict(model2, newdata = .))

# Add to plot
scatter_plot +
  geom_line(data = pred_data1, aes(x = total_rainfall, y = fitted), color = "blue", linewidth = 1) +
  geom_line(data = pred_data2, aes(x = total_rainfall, y = fitted), color = "red", linewidth = 1)

