# This is the script for Lab Week 09

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_02/week_01")
getwd() # Confirm I am working in the proper directory.

# Load required libraries using pacman for convenience
# pacman will check if the package is installed, install it if not, and then load it for use
library(pacman)
p_load(dplyr,readr,tidyverse,ggplot2,modelsummary,GGally,factoextra,pandoc)

# =======================================================
# READ IN DATA
# =======================================================

# Read in the shopper_info dataset
# This dataset contains detailed information on shoppers and their transactions for July 2023
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_info.csv")

# Read in the GTIN dataset
# This file links products to their Global Trade Item Numbers, akin to SKUs or UPCs
gtin <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/gtin.csv")

# Read in the store_info dataset
# Contains details about each store, linkable to shopper_info via store_id
store_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/store_info.csv")

head(shopper_info)
head(gtin)
head(store_info)

# =======================================================
# JOINS
# =======================================================

# Join shopper_info and store_info by the common key "store_id"
shopper_store_inner <- inner_join(shopper_info, store_info, by = "store_id")

# Keeping all rows from shopper_info and adding information from store_info where possible
shopper_store_left <- left_join(shopper_info, store_info, by = "store_id")

# Keeping all rows from store_info and adding information from shopper_info where possible
store_shopper_right <- right_join(shopper_info, store_info, by = "store_id")

# Combining all information from both shopper_info and store_info
shopper_store_full <- full_join(shopper_info, store_info, by = "store_id")

# =======================================================
# YOU DO IT
# =======================================================

shopper_store_left <- left_join(shopper_info, store_info, by = "store_id")

total_observations <- nrow(shopper_store_left)

print(total_observations)

write_csv(shopper_store_left, "shopper_store_left.csv") 

# =======================================================
# SUMMARY STATISTICS
# =======================================================

# Using our dataframe 'shopper_store_full'
datasummary_skim(shopper_store_full, type = "numeric")
datasummary_skim(shopper_store_full, type = "categorical")

datasummary(unit_price + unit_quantity + chain_size ~ Mean + SD + Min + Max,
            data=shopper_store_full,
            output = "sumstats.docx")

# =======================================================
# GGPLOT
# =======================================================

# Example: choose numeric variables relevant to your research question
shopper_store_left %>%
  filter(unit_price > 0) %>% # Drop observations where unit price is negative (these are returns)
  filter(!is.na(gtin), gtin!=0) %>% # Drop observations where gtin is NA (which means fuel purchases) or gtin = 0
  select(unit_price, unit_quantity, chain_size) %>% 
  mutate(total = unit_price*unit_quantity) %>% # Create a "total" variable using `mutate()` 
  select(total, chain_size) %>% 
  ggpairs()

# =======================================================
# CLUSTER ANALYSIS
# =======================================================

clean_data <- shopper_store_left %>%
  filter(unit_price > 0) %>% # Drop observations where unit price is negative (these are returns)
  filter(!is.na(gtin), gtin!=0) %>% # Drop observations where gtin is NA (which means fuel purchases) or gtin = 0
  mutate(total = unit_price*unit_quantity) %>%
  mutate(log_chain_size = log(chain_size + 1)) %>% # example transformation
  arrange(shopper_id, store_id, transaction_set_id) %>%
  drop_na() 
  
cluster_data <- clean_data %>%
  select(log_chain_size, total) 

cluster_scaled <- scale(cluster_data)

# Silhouette method
fviz_nbclust(cluster_scaled, kmeans, method = "silhouette")

# Elbow method
fviz_nbclust(cluster_scaled, kmeans, method = "wss")

# K-means clustering
set.seed(123)
kmeans_fit <- kmeans(cluster_scaled, centers = 3, nstart = 25)

final_clusters <- clean_data %>%
  mutate(cluster = kmeans_fit$cluster)

final_clusters %>%
  group_by(cluster) %>%
  summarise(across(c(chain_size, total), mean))

