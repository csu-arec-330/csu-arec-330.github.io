# This is the script for Lab Week 11

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_02/week_03")
getwd() # Confirm I am working in the proper directory.

# Load required libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(factoextra)
library(readr)
library(knitr)

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

# Join the data frames together
store_shopper_gtin_left <- left_join(store_info, shopper_info, by = "store_id") %>%
  left_join(gtin, by = "gtin")

# Look at the raw count of observations after join
total_observations <- nrow(store_shopper_gtin_left)
print(paste0("Total observations of raw data: ", total_observations))

# Look at the number of distinct store id's prior to cleaning the data
store_count_raw <- length(unique(store_shopper_gtin_left$store_id))
print(paste0("Count of distinct stores in raw data: ", store_count_raw))

# Clean and transform the raw data
clean_data <- store_shopper_gtin_left %>%
  
  # Remove observations with negative or zero unit price (e.g., returns or invalid entries)
  filter(unit_price > 0) %>% 
  
  # Remove observations of stores missing shopper IDs
  filter(!is.na(shopper_id)) %>%
  
  # Calculate total spending per line item
  mutate(total = unit_price * unit_quantity) %>%
  
  # Convert transaction_set_id and transaction_item_id to integers without decimals
  mutate(transaction_set_id = format(transaction_set_id, scientific = FALSE, trim = TRUE), 
         transaction_item_id = format(transaction_item_id, scientific = FALSE, trim = TRUE)) %>%
  
  # Arrange data by store_id and zip_code
  arrange(store_id, zip_code)

# Check how many distinct shoppers are in the clean dataset
unit_of_analysis <- length(unique(clean_data$store_id))
print(paste0("Unit of analysis (store-level): ", unit_of_analysis))

# Summarize metrics at the store level
store_summary <- clean_data %>%
  group_by(store_id) %>%
  summarise(
    total_sales = sum(unit_price * unit_quantity, na.rm = TRUE),               # Total revenue
    unique_customers = n_distinct(shopper_id),                                 # Number of unique customers
    product_diversity = n_distinct(gtin),                                      # Number of unique products sold
    fuel_transactions = sum(is.na(gtin) | gtin == 0),                          # Number of fuel transactions
    total_transactions = n_distinct(transaction_set_id),                       # Total number of transactions
    fuel_share = fuel_transactions / total_transactions,                       # Fuel share of transactions
    .groups = "drop"
  ) %>%
  left_join(store_info %>% select(store_id, chain_size), by = "store_id") %>%      # Add chain size
  filter(!is.na(chain_size))

# Final dataset: merge summaries if desired
final_dataset <- store_summary %>%
  left_join(store_info %>% select(-chain_size), by = c("store_id")) %>%
  arrange(store_id, zip_code)

sumstats <- store_summary %>%
  summarise(
    mean_sales = mean(total_sales),
    mean_customers = mean(unique_customers),
    mean_products = mean(product_diversity),
    mean_fuel_share = mean(fuel_share),
    mean_chain_size = mean(chain_size),
    .groups = "drop"
  )

# I noticed that the distribution of my store attributes is very skewed, so I want to rescale them.
final_dataset <- final_dataset %>%
  mutate(
    log_customers = log(unique_customers + 1),
    log_sales = log(total_sales + 1),
    log_products = log(product_diversity + 1),
    log_chain_size = log(chain_size + 1)
  )

# Run the ggpairs() command
final_dataset %>%
  select(log_customers, log_sales, log_products, fuel_share, log_chain_size) %>% 
  ggpairs()

# Prepare data for clustering
cluster_data <- final_dataset %>%
  select(log_customers, log_sales, log_products, fuel_share, log_chain_size)

cluster_scaled <- scale(cluster_data)

# Use two common methods to help choose the number of clusters:
# Elbow Method
fviz_nbclust(cluster_scaled, kmeans, method = "wss")

# Silhouette Method
fviz_nbclust(cluster_scaled, kmeans, method = "silhouette")

# Perform k-means clustering on logged and scaled data
set.seed(123)
kmeans_fit <- kmeans(cluster_scaled, centers = 4, nstart = 25)

# Add cluster to dataset
final_dataset$cluster <- factor(kmeans_fit$cluster)

fviz_cluster(kmeans_fit, data = cluster_scaled, geom = "point", ellipse.type = "norm")

# Use the original (non-logged) variables because you're summarizing actual store characteristics — not the transformed versions.
final_clusters <- final_dataset %>%
  group_by(cluster) %>%
  summarize(across(c(unique_customers, total_sales, product_diversity, fuel_share, chain_size), mean))

print(final_clusters)

final_dataset %>%
  select(cluster, city) %>%
  group_by(cluster, city) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  slice_max(order_by = n, n = 5) %>%
  ungroup() %>%
  kable()

# =======================================================
# WHAT DO YOUR CLUSTERS REVEAL?
# =======================================================

# Join cluster labels to store_info (keeping only cluster info)
most_frequent_purchase <- store_info %>%
  # Join cluster info only
  left_join(final_dataset %>% select(store_id, cluster),
            by = c("store_id")) %>%
  
  # Filter out rows with missing cluster
  filter(!is.na(cluster)) %>%
  
  # Join in shopper info
  left_join(shopper_info, by = "store_id") %>%
  
  # Join in produt info
  left_join(gtin, by = "gtin") %>%
  
  # Count purchases by cluster and subcategory
  group_by(cluster, gtin, subcategory) %>%
  filter(!is.na(subcategory)) %>%
  summarize(purchase_count = n(), .groups = "drop") %>%
  
  # Get the top subcategory in each cluster
  group_by(cluster) %>%
  filter(purchase_count == max(purchase_count)) %>%
  ungroup() %>%
  select(-gtin)

View(most_frequent_purchase)

# Join most_frequent_purchase with final_clusters and then write to csv
final_clusters_out <- final_dataset %>%
  left_join(most_frequent_purchase, by = "cluster")

write_csv(final_clusters_out, "final_clusters_out.csv")
