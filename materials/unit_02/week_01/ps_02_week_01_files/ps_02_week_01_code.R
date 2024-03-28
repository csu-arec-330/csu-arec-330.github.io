
# Load necessary libraries
library(pacman)
p_load(dplyr,readr,tidyverse,ggplot2,GGally,broom,ranger,rsample,caret,skimr)

# Load the data
data <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/arizona_grocery_foot_traffic.csv")

# 1. Generate a table of summary statistics. The table should include at least: variable names, mean (average), standard deviation, min, and max.
summary_stats <- data %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), 
                                      sd = ~sd(.x, na.rm = TRUE),
                                      min = ~min(.x, na.rm = TRUE), 
                                      max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c(".value", "variable"), names_sep = "_")

print(summary_stats)

# 2. Perform any necessary processing (e.g., removing outliers). Write a narrative of your processing steps justifying your decisions.

Q1 <- quantile(data$variable, 0.25)
Q3 <- quantile(data$variable, 0.75)
IQR <- Q3 - Q1

# Filter out outliers
filtered_data <- data %>%
  filter(variable >= (Q1 - 1.5 * IQR) & variable <= (Q3 + 1.5 * IQR))



# 3. Write a narrative for your webpage describing your EDA and providing an interpretation of your data. Describe your data so that readers understand what is measured. See the Canvas assignment for a codebook describing the data. Here are some prompt questions for your narrative:

# Which grocery store has the most foot traffic in AZ? Is it the same with the most unique visitors?

most_traffic <- data %>%
  group_by(store_name) %>%
  summarise(total_foot_traffic = sum(foot_traffic, na.rm = TRUE)) %>%
  arrange(desc(total_foot_traffic)) %>%
  top_n(1)

print(most_traffic)

# What is the most common store type (in your data) in AZ?

most_common_store_type <- data %>%
  group_by(store_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(1, count)

print(most_common_store_type)

# Which city has the most stores?

city_with_most_stores <- data %>%
  group_by(city) %>%
  summarise(number_of_stores = n_distinct(store_name)) %>%
  arrange(desc(number_of_stores)) %>%
  top_n(1, number_of_stores)

print(city_with_most_stores)
  
# Which store do people travel the farthest to visit?

farthest_store <- data %>%
  group_by(store_name) %>%
  summarise(average_distance = mean(distance_traveled, na.rm = TRUE)) %>%
  arrange(desc(average_distance)) %>%
  slice(1)

print(farthest_store)


# 4. Use K-means clustering to cluster on multiple numeric dimensions in your data. Note that this does not mean run the clustering algorithm multiple times. Explain why you clustered on the dimensions you chose and what the clusters tell you. Try a few different cluster sizes and choose one that makes sense. Write a narrative in your webpage explaining your cluster analysis and what you learned.

# Load necessary library
library(stats)

# Choose relevant columns for clustering
clustering_data <- data %>%
  select(numeric_variable_1, numeric_variable_2) # Update with your chosen variables

# Perform K-means clustering
set.seed(123) # Ensure reproducibility
clusters <- kmeans(clustering_data, centers = 3) # Experiment with different numbers of centers

# Analyze clusters
print(clusters$size)
print(clusters$centers)
