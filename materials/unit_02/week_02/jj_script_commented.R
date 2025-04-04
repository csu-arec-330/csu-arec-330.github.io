setwd("C:/Users/Jhoany Juarez/OneDrive - Colostate/AREC330/project_2/problem_set_1")

library(pacman)
p_load(dplyr, readr, tidyverse, ggplot2, modelsummary, GGally, factoextra, pandoc)

# Load datasets (assuming you've already loaded these) << You don't need to download them with read_csv. You can feed it the URLs from our course website
# shopper_info <- read_csv("shopper_info (1).csv")
# gtin <- read_csv("gtin (1).csv")
# store_info <- read_csv("store_info (1).csv")

# LC: Use the URLs from our lab notes, so you don't have to download the CSV files. 
shopper_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/shopper_info.csv")
gtin <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/gtin.csv")
store_info <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/store_info.csv")

# LC: I'm sharing this file with everyone today. It is a separate data file with shopper ID and their zipcode
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
  
  # mutate(log_chain_size = log(chain_size + 1)) %>%  # LC: Let's remove this for now. Later in the code, we can log transform the variables.
  
  arrange(shopper_id, store_id, transaction_set_id) 

# LC: Let's remove this too. What it does is delete any observation where at least one variable contains an "NA".
# %>%
#   
#   drop_na()

#============================================================================
# Identify Alcohol Preferences
#============================================================================

# LC: This line of code shows you all the distinct subcategories in your clean data.
unique(clean_data$subcategory)

# LC: You discovered grepl! Awesome! This is such a helpful command to do what's called a "string search" inside the values of the variables. It's looking for any appearances of the text "alcohol" in subcategory. However, after looking at the unique values of subcategory, none of them have the text "alcohol" so you will get zero observations of alcohol-related gtins. Instead, let's look at the category column and see if we can filter on certain categories instead.
alcoholic_gtin <- clean_data %>% # Renaming this alcohol_transactions
  filter(grepl("alcohol", subcategory, ignore.case = TRUE))  

# LC: This line of code shows you all the distinct categories in your clean data.
unique(clean_data$category)

# LC: Bingo! Beer, Wine, and Liquor are all categories, so let's filter alcohol transactions like this instead:
alcoholic_transactions <- clean_data %>% # Renaming this alcohol_transactions
  filter(category == "Beer" | category == "Wine" | category == "Liquor")  

# LC: Comment this out because you won't need it.
# alcoholic_transactions <- clean_data %>%
#   filter(gtin %in% alcoholic_gtin$gtin)

# LC: At this stage, it's a good idea to take a look at your data.
View(alcoholic_transactions)

#============================================================================
# Join Alcoholic Transactions with Store Information
#============================================================================

# LC: You already have geographic identifiers in your alcoholic transactions dataframe so you don't need to re-merge it. 
# # Join the alcohol transactions with store info to get geographical location
# alcoholic_transactions_geo <- alcoholic_transactions %>%
#   left_join(store_info, by = "store_id")
# 
# 
# colnames(store_info)  

#============================================================================
# Group by Geographical Location and Shopper
#============================================================================

geo_segmented <- alcoholic_transactions %>% # LC: Made an adjustment here to just feed it "alcoholic_transactions"
  group_by(city, shopper_id) %>% # LC: Made an adjustment here to "city"
  summarize(
    total_spent = sum(total, na.rm = TRUE),
    avg_items = mean(unit_quantity, na.rm = TRUE),
    num_visits = n(),
    .groups = "drop"
  )
# LC: The end result here is a dataframe with total spent, average number of items, and number of visits, by shopper and city

#============================================================================
# Segment Customers by Spending Habits
#============================================================================

spending_habits <- geo_segmented %>%
  mutate(spending_segment = case_when(
    total_spent < 50 ~ "Low Spender",
    total_spent >= 50 & total_spent < 200 ~ "Medium Spender",
    total_spent >= 200 ~ "High Spender"
  ))
# LC: Clever use of chase_when!

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

# LC: Made a couple of updates here: 1) replaced alcoholic_transactions_geo with alcoholic_transactions, 2) used a left join by shopper id and city to bring in the results from the cluster analysis
alcohol_pref_clusters <- alcoholic_transactions %>% # LC: Updated the data frame because we didn't need to create alcoholic_transactions_geo
  left_join(spending_habits_clustered, by = c("shopper_id", "city")) 

# LC: Don't need this
# %>%
#   filter(!is.na(gtin))  

alcohol_pref_summary <- alcohol_pref_clusters %>%
  group_by(cluster, zip_code) %>% # LC: YOu had "store_region" here but that's not in the data. So we'll want to replace this with something else -- how about zipcode?
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
  
  left_join(spending_habits_clustered %>% select(shopper_id, city, cluster), # LC: Removed transaction_set_id 
            by = c("shopper_id", "city")) %>% # LC: "store_id", "transaction_set_id" are not in spending_habits_clustered so let's replace with "city"
  
  
  filter(!is.na(gtin), gtin != 0, !is.na(cluster)) %>%
  
  
  # left_join(gtin, by = "gtin") %>% # LC: Don't need this -- it's already in the clean data
  
  
  group_by(cluster, gtin, subcategory, category) %>% # LC: Added category so you can see what major category it is
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
