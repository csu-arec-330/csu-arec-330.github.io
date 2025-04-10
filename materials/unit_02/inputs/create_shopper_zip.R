# Create shopper demographic file assigning each shopper to a home zip code

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_02/inputs")
getwd() # Confirm I am working in the proper directory.

# Load required libraries using pacman for convenience
# pacman will check if the package is installed, install it if not, and then load it for use
#install.packages("pacman")
library(pacman)
p_load(dplyr,readr,tidyverse,ggplot2,modelsummary,GGally,factoextra,pandoc)

# Read in the shopper_info dataset
# This dataset contains detailed information on shoppers and their transactions for July 2023
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_info.csv")

# Separate data file with shopper ID and their zipcode
shopper_zip <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_zip.csv")

# Read in the GTIN dataset
# This file links products to their Global Trade Item Numbers, akin to SKUs or UPCs
gtin <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/gtin.csv")

# Read in the store_info dataset
# Contains details about each store, linkable to shopper_info via store_id
store_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/store_info.csv")

# Join files
shopper_store_gtin_left <- left_join(shopper_info, store_info, by = "store_id") %>%
  left_join(gtin, by = "gtin")

length(unique(shopper_store_gtin_left$shopper_id))

# Create a shopper demographic file with zip codes
shopper_zip <- shopper_store_gtin_left %>%
  select(shopper_id, zip_code) %>%
  distinct() %>%
  filter(!is.na(zip_code)) %>%         # remove NAs
  group_by(shopper_id, zip_code) %>%
  summarise(
    count = n(), 
    .groups = "drop"
  ) %>%
  group_by(shopper_id) %>%
  filter(count == max(count)) %>%      # keep the most frequent zip_code
  slice(1) %>%                         # if there's a tie, just take the first one
  ungroup() %>%
  select(shopper_id, zip_code) %>%        # drop count column
  mutate(shopper_zip = substr(zip_code, 1, 5)) %>%
  arrange(shopper_zip) %>%
  select(-zip_code)

write_csv(shopper_zip, "shopper_zip.csv")


# =================================================
# shopper_zip <- read_csv("shopper_zip_v1.csv") # original file

library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
library(ggplot2)

# Replace with your actual path
zcta_path <- "cb_2020_us_zcta520_500k/cb_2020_us_zcta520_500k.shp"

zctas <- st_read(zcta_path)
names(zctas)

# Make sure shopper ZIPs are character
shopper_zip$shopper_zip <- as.character(shopper_zip$shopper_zip)
shopper_zip$shopper_zip <- sprintf("%05s", shopper_zip$shopper_zip)  # pad ZIPs with leading 0s if needed

zctas$GEOID20 <- as.character(zctas$GEOID20)

# Add a flag to show which ZIPs have at least one shopper
zctas$has_shopper <- ifelse(zctas$GEOID20 %in% shopper_zip$shopper_zip, "Yes", "No")
table(zctas$has_shopper)

# Plot zip codes with at least one shopper
ggplot(zctas) +
  geom_sf(aes(fill = has_shopper), color = NA) +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "grey90")) +
  theme_minimal() +
  labs(title = "ZIP Codes with At Least One Shopper")


# Define a bounding box for the contiguous US
# Approximate bounds: xmin, ymin, xmax, ymax
contig_us_bbox <- st_bbox(c(xmin = -125, ymin = 24, xmax = -66, ymax = 50), crs = st_crs(zctas))

# Crop the ZCTAs to that box
zctas_contig <- st_crop(zctas, contig_us_bbox)

# Plot zip codes with at least one shopper - lower 48
ggplot(zctas_contig) +
  geom_sf(aes(fill = has_shopper), color = NA) +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "grey90")) +
  theme_minimal() +
  labs(title = "Contiguous U.S. ZIP Codes with Shoppers")
