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

shopper_store_gtin_left <- left_join(shopper_store_left, gtin, by = "gtin")

total_observations <- nrow(shopper_store_gtin_left)

print(total_observations)

length(unique(shopper_store_gtin_left$shopper_id))

write_csv(shopper_store_gtin_left, "shopper_store_gtin_left.csv") 

# =======================================================
# CLEAN DATA
# =======================================================

# Clean and transform the raw data
clean_data <- shopper_store_gtin_left %>%
  # Remove observations with negative or zero unit price (e.g., returns or invalid entries)
  filter(unit_price > 0) %>% 
  
  # Drop observations with missing or invalid GTINs (e.g., fuel purchases or non-product transactions)
  filter(!is.na(gtin), gtin != 0) %>%
  
  # Calculate total spending per line item
  mutate(total = unit_price * unit_quantity) %>%
  
  # Log-transform chain size to reduce skewness and interpret relative differences
  mutate(log_chain_size = log(chain_size + 1)) %>%
  
  # Arrange data to ensure proper ordering of transactions
  arrange(shopper_id, store_id, transaction_set_id) %>%
  
  # Drop remaining rows with NA values across key variables
  drop_na()

# Create visit-level summaries (per shopper-store-visit)
visit_summary <- clean_data %>%
  group_by(shopper_id, store_id, transaction_set_id) %>%
  summarize(
    total_spent = sum(total, na.rm = TRUE),         # Total spending per visit
    avg_items = mean(unit_quantity, na.rm = TRUE),  # Average items per basket (per GTIN)
    .groups = "drop"
  ) %>%
  # Optional: Remove extreme outliers in total_spent (e.g., top 0.1%) to avoid skewing summaries
  filter(total_spent < quantile(total_spent, 0.999))

# Create shopper-level summary of store visit frequency
visit_frequency <- clean_data %>%
  distinct(shopper_id, store_id, transaction_set_id) %>%
  group_by(shopper_id, store_id) %>%
  summarize(
    num_visits = n(),  # Frequency of visits to each store
    .groups = "drop"
  )

# Final dataset: merge summaries if desired
final_dataset <- visit_summary %>%
  left_join(visit_frequency, by = c("shopper_id", "store_id")) %>%
  left_join(store_info, by = "store_id") %>%
  arrange(shopper_id, store_id)

length(unique(shopper_store_gtin_left$shopper_id))
length(unique(final_dataset$shopper_id))

View(final_dataset)


# =======================================================
# SUMMARY STATISTICS
# =======================================================

# Using our dataframe 'final_dataset'
datasummary_skim(final_dataset, type = "numeric")
datasummary_skim(final_dataset, type = "categorical")

datasummary(total_spent + avg_items + num_visits ~ Mean + SD + Min + Max,
            data=final_dataset,
            output = "sumstats.docx")

# =======================================================
# GGPLOT
# =======================================================

# Example: choose numeric variables relevant to your research question
# These will be `total_spent`, `avg_items`, and `num_visits`
final_dataset %>%
  select(total_spent, avg_items, num_visits) %>% 
  ggpairs()

# =======================================================
# CLUSTER ANALYSIS
# =======================================================

cluster_data <- final_dataset %>%
  select(total_spent, avg_items, num_visits) 

cluster_scaled <- scale(cluster_data)

# Silhouette method
fviz_nbclust(cluster_scaled, kmeans, method = "silhouette")

# Elbow method
fviz_nbclust(cluster_scaled, kmeans, method = "wss")

# K-means clustering
set.seed(123)
kmeans_fit <- kmeans(cluster_scaled, centers = 3, nstart = 25)

final_clusters <- final_dataset %>%
  mutate(cluster = kmeans_fit$cluster)

final_clusters %>%
  group_by(cluster) %>%
  summarize(across(c(total_spent, avg_items, num_visits), mean))

View(final_clusters)

# =======================================================
# WHAT DO YOUR CLUSTERS REVEAL?
# =======================================================

# Join cluster labels to shopper_info (keeping only cluster info)
most_frequent_purchase <- shopper_info %>%
  # Join cluster info only
  left_join(final_clusters %>% select(shopper_id, store_id, transaction_set_id, cluster),
            by = c("shopper_id", "store_id", "transaction_set_id")) %>%
  
  # Filter out rows with missing/invalid gtin or missing cluster
  filter(!is.na(gtin), gtin != 0, !is.na(cluster)) %>%
  
  # Join in product info
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

