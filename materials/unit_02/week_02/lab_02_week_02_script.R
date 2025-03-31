# This is the script for Lab Week 10

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_02/week_02")
getwd() # Confirm I am working in the proper directory.

# Load required libraries using pacman for convenience
# pacman will check if the package is installed, install it if not, and then load it for use
#install.packages("pacman")
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
# STORE SEGMENTATION
# =======================================================

store_shopper_left <- left_join(store_info, shopper_info, by = "store_id")

total_observations <- nrow(store_shopper_left)

print(total_observations)

store_shopper_gtin_left <- left_join(store_shopper_left, gtin, by = "gtin")

total_observations <- nrow(store_shopper_gtin_left)

print(total_observations)

length(unique(store_shopper_gtin_left$store_id))

# =======================================================
# CLEAN DATA
# =======================================================

# Clean and transform the raw data
clean_data <- store_shopper_gtin_left %>%
  # Remove observations with negative or zero unit price (e.g., returns or invalid entries)
  filter(unit_price > 0) %>% 
  
  # Drop observations with missing or GTIN==0, but I want to KEEP the fuel purchases, so I will keep the "NA" values
  filter(!is.na(shopper_id)) %>%
  
  # Calculate total spending per line item
  mutate(total = unit_price * unit_quantity) %>%
  
  # Arrange data by store_id and zip_code
  arrange(store_id, zip_code)

length(unique(clean_data$store_id))

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

View(final_dataset)


# =======================================================
# SUMMARY STATISTICS
# =======================================================

# Using our dataframe 'final_dataset'
datasummary_skim(final_dataset, type = "numeric")
datasummary_skim(final_dataset, type = "categorical")

datasummary(unique_customers + total_sales + product_diversity + fuel_share + chain_size ~ Mean + SD + Min + Max,
            data=final_dataset,
            output = "sumstats.docx")

# =======================================================
# GGPLOT
# =======================================================

# Example: choose numeric variables relevant to your research question
final_dataset %>%
  select(unique_customers, total_sales, product_diversity, fuel_share, chain_size) %>% 
  ggpairs()

ggsave("includes/ggpairs_store_segment_levels.png")

# I noticed that the distribution of my store attributes is very skewed, so I want to rescale them.
final_dataset <- final_dataset %>%
  mutate(
    log_customers = log(unique_customers + 1),
    log_sales = log(total_sales + 1),
    log_products = log(product_diversity + 1),
    log_chain_size = log(chain_size + 1)
  )

# Run the ggpairs() command again
final_dataset %>%
  select(log_customers, log_sales, log_products, fuel_share, log_chain_size) %>% 
  ggpairs()

ggsave("includes/ggpairs_store_segment.png")

# =======================================================
# CLUSTER ANALYSIS
# =======================================================

# Check for NAs in your log-transformed variables
final_dataset %>%
  select(log_customers, log_sales, log_products, fuel_share, chain_size) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

cluster_data <- final_dataset %>%
  select(log_customers, log_sales, log_products, fuel_share, log_chain_size) 

cluster_scaled <- scale(cluster_data)

# Silhouette method
fviz_nbclust(cluster_scaled, kmeans, method = "silhouette")

# Elbow method
fviz_nbclust(cluster_scaled, kmeans, method = "wss")

# K-means clustering
set.seed(123)
kmeans_fit <- kmeans(cluster_scaled, centers = 4, nstart = 25)

final_clusters <- final_dataset %>%
  mutate(cluster = kmeans_fit$cluster)

final_clusters %>%
  group_by(cluster) %>%
  summarize(across(c(unique_customers, total_sales, product_diversity, fuel_share, chain_size), mean))

View(final_clusters)

# =======================================================
# WHAT DO YOUR CLUSTERS REVEAL?
# =======================================================

# Join cluster labels to store_info (keeping only cluster info)
most_frequent_purchase <- store_info %>%
  # Join cluster info only
  left_join(final_clusters %>% select(store_id, cluster),
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

