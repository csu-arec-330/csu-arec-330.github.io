# This is the R script for Week 13 Lab. 
# This script will demonstrate how to work with spatial data and conduct some basic operations (e.g., intersection)

# load packages
library(pacman)
p_load(tidyverse,janitor,sf,tigris,mapview,tidycensus,dplyr)

# Read in convenience store location dataset
store_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/store_info.csv.gz")

# Convert data frame to a points simple feature object
store_geo <- store_raw %>%
  
  # First, we subset the variables `store_id` and the coordinates.
  select(store_id, latitude, longitude) %>% 
  
  # Tells R that these are essentially $x$ and $y$ coordinates in a certain projection (represented by the code 4326). See <https://epsg.io/> for more information.
  st_as_sf(coords=c("longitude","latitude"), crs = st_crs(4326))  

# Configure mapview to render points directly in the RStudio Viewer (not using flatgeobuf)
mapviewOptions(fgb = FALSE)

# Display the store locations as an interactive map
mapview(store_geo)

# Fetch data for county polygons using the 'tigris' package
us_co <- tigris::counties(cb=T,class="sf") %>%
  janitor::clean_names() %>%
  mutate(aland=aland/2.59e+6) %>%
  st_transform(4326)

mapview(us_co)

# Look at the distinct state list by statefp code
unique_statefp <- us_co %>%
  st_set_geometry(NULL) %>%  # Remove geometry column
  select(stusps, statefp) %>%  # Select the columns of interest
  distinct() %>%  # Remove duplicates, keeping only unique rows
  arrange(stusps)  # Arrange alphabetically by stusps

# Filter out counties from American Samoa (60), Guam (66), Saipan Municipality (69), Puerto Rico (72), Virgin Islands (78), Alaska (02), and Hawaii (15)
us_co_filtered <- us_co %>%
  filter(!statefp %in% c("60", "66", "72", "02", "15", "69", "78"))

# Use mapview to plot the county layer, excluding the specified states
mapview(us_co_filtered)

# Join county to store_geo
store_co_geo <- st_join(store_geo, us_co_filtered, join=st_intersects)

# Aggregate store count by county
store_count_by_county <- store_co_geo %>%
  group_by(geoid) %>%
  summarize(store_count = n(), .groups = 'drop')  # Drop groups to prevent regrouping

# Join aggregated data back with county geometries for mapping
county_store_map <- st_join(us_co_filtered,store_count_by_county,join=st_intersects)

# Visualize the result with mapview (showing number of stores per county)
mapview(county_store_map, zcol = "store_count")

# Load variable metadata for the 2022 ACS 5-year estimates (used to look up variable codes and labels)
census_22 <- load_variables(2022, "acs5", cache = TRUE)

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

# Join median household income (hhi) to the store-level dataset using county GEOID
store_hhi <- store_co_geo %>%
  st_set_geometry(NULL) %>%        # Remove geometry to work with as a regular data frame
  inner_join(hhi, by = "geoid")    # Join on county identifier (GEOID)

# Count the number of stores per county by joining spatial store data with county identifiers
county_stores <- store_co_geo %>%
  st_set_geometry(NULL) %>%            # Drop geometry to simplify data manipulation
  group_by(geoid, county = name, state = stusps) %>%  # Group by county GEOID, name, and state abbreviation
  summarize(total_stores = n()) %>%    # Count the number of stores in each county
  ungroup()                            # Remove grouping to avoid unexpected behavior in downstream steps

# Read in the transaction level data
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/shopper_info.csv.gz")

# Read in the weather data
weather_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_03/inputs/gridmet_county_daily_2023-2023.csv.gz")

# Aggregate daily sales by store from transaction-level data
store_sales <- shopper_info %>%
  mutate(measure_date = as_date(date_time)) %>%              # Extract date component from timestamp
  group_by(store_id, measure_date) %>%                       # Group by store and date
  summarize(
    sales = sum(unit_price * unit_quantity, na.rm = TRUE)   # Compute total sales (price × quantity)
  ) %>%
  ungroup()                                                  # Remove grouping to clean up the result
# Add county GEOID to the store-level sales data (so we can link to county-level data like weather)
store_sales_co <- st_set_geometry(store_co_geo, NULL) %>%
  select(store_id, geoid) %>%                      # Keep only store_id (for joining) and geoid (for county-level linkage)
  inner_join(store_sales, ., by = "store_id")      # Join to sales data using store_id; keep only stores with known county info

# Reshape weather data from long to wide format (each variable becomes a column)
weather_wide <- weather_raw %>%
  pivot_wider(
    id_cols = c(county, date),              # Keep county and date as identifiers
    names_from = variable,                  # Each unique variable becomes a new column
    values_from = value                     # Fill those columns with the corresponding values
  )

# Join daily store sales to weather data using date and county identifiers
store_sales_weather <- store_sales_co %>%
  inner_join(weather_wide, by = c("measure_date" = "date", "geoid" = "county"))  # Match on date and county FIPS
