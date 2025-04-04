setwd("C:/Users/Jhoany Juarez/OneDrive - Colostate/AREC330/project_2/problem_set_1")

library(pacman)
p_load(dplyr, readr, tidyverse, ggplot2, modelsummary, GGally, factoextra, pandoc)

# Load datasets. Use the URLs from our lab notes, so you don't have to download the CSV files. 
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_info.csv")
gtin <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/gtin.csv")
store_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/store_info.csv")
shopper_zip <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_zip.csv")


#============================================================================
# Join Datasets
#============================================================================
shopper_store_left <- left_join(shopper_info, store_info, by = "store_id")

shopper_store_gtin_left <- left_join(shopper_store_left, gtin, by = "gtin")

#============================================================================
# Clean and Transform Data
#============================================================================

clean_data <- shopper_store_gtin_left %>%
  
  filter(unit_price > 0) %>%
  
  filter(!is.na(gtin), gtin != 0) %>%
  
  mutate(total = unit_price * unit_quantity) %>%
  
  arrange(shopper_id, store_id, transaction_set_id) 

#============================================================================
# Identify Alcohol Preferences
#============================================================================

# This line of code shows you all the distinct subcategories in your clean data.
unique(clean_data$subcategory)

# This line of code shows you all the distinct categories in your clean data.
unique(clean_data$category)

# Filter out Beer, Wine, and Liquor categories
alcoholic_transactions <- clean_data %>%
  filter(category == "Beer" | category == "Wine" | category == "Liquor")  

View(alcoholic_transactions)



#============================================================================
# Group by Geographical Location and Shopper
#============================================================================

geo_segmented <- alcoholic_transactions %>% 
  group_by(city, shopper_id) 
  summarize(
    total_spent = sum(total, na.rm = TRUE),
    avg_items = mean(unit_quantity, na.rm = TRUE),
    num_visits = n(),
    .groups = "drop"
  )
# The end result here is a dataframe with total spent, average number of items, and number of visits, by shopper and city

#============================================================================
# Segment Customers by Spending Habits
#============================================================================

spending_habits <- geo_segmented %>%
  mutate(spending_segment = case_when(
    total_spent < 50 ~ "Low Spender",
    total_spent >= 50 & total_spent < 200 ~ "Medium Spender",
    total_spent >= 200 ~ "High Spender"
  ))


#============================================================================
# Clustering Customers
#============================================================================

cluster_data <- spending_habits %>%
  select(total_spent, avg_items, num_visits)

cluster_scaled <- scale(cluster_data)

#I was able to run all of this up to here and then I just continued to write it out how I belive it should be 
fviz_nbclust(cluster_data, kmeans, method = "silhouette")
fviz_nbclust(cluster_data, kmeans, method = "wss")


set.seed(123)
kmeans_fit <- kmeans(cluster_scaled, centers = 3, nstart = 25)

spending_habits_clustered <- spending_habits %>%
  mutate(cluster = kmeans_fit$cluster)

spending_habits_clustered %>%
  group_by(cluster) %>%
  summarize(
    avg_spent = mean(total_spent, na.rm = TRUE),
    avg_items = mean(avg_items, na.rm = TRUE),
    avg_visits = mean(num_visits, na.rm = TRUE) )

#============================================================================
# Combine Alcohol Preferences with Clusters and Geographical Data
#============================================================================

alcohol_pref_clusters <- alcoholic_transactions %>% 
  left_join(spending_habits_clustered, by = c("shopper_id", "city")) 

alcohol_pref_summary <- alcohol_pref_clusters %>%
  group_by(cluster, zip_code) %>% 
  summarize(
    alcohol_spent = sum(total, na.rm = TRUE),
    alcohol_items = sum(unit_quantity, na.rm = TRUE),
    num_visits = n(),
    .groups = "drop")

View(alcohol_pref_summary)

#============================================================================
# Insights and Trends
#============================================================================

View(spending_habits_clustered)

most_frequent_purchase <- clean_data %>% # LC: Replace shopper_info (the raw data) with the clean data
  
  left_join(spending_habits_clustered %>% select(shopper_id, city, cluster), 
            by = c("shopper_id", "city")) %>% 
  
  filter(!is.na(gtin), gtin != 0, !is.na(cluster)) %>%
  
  group_by(cluster, gtin, subcategory, category) %>% 
  filter(!is.na(subcategory)) %>%
  summarize(
    purchase_count = n(),
    .groups = "drop") %>%
  
  group_by(cluster) %>%
  filter(purchase_count == max(purchase_count)) %>%
  ungroup() %>%
  select(-gtin)

View(most_frequent_purchase)

# LC: End result -- 
# 1) You used cluster analysis on your alcohol purchasers grouping them by total spend, average items purchased, and number of items bought. Dataframe = spending_habits_clustered.
# 2) You then summarized their preferences by cluster and zipcode. Dataframe = alcohol_pref_summary
# 3) You then brought this information back into the clean data to see what the most frequent purchases are across clusters, regardless of whether it was alcohol purchases. Dataframe = most_frequent_purchase
