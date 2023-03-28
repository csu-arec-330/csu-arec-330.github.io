#clustering example

# load packages
library(tidyverse)
library(ggplot2)
library(GGally)

# read in dataset
raw <- read_csv("~/Downloads/arizona_grocery_foot_traffic.csv")

# select relevant columns
data <- raw %>% 
  dplyr::filter(top_category=="Grocery Stores") %>%
  select(placekey,raw_visitor_counts,distance_from_home,median_dwell) %>%
  mutate(across(c(2:4),log)) %>%
  drop_na()

ggpairs(data,columns = 2:4)

# scale data
data_scaled <- data %>%
  select(raw_visitor_counts,distance_from_home,median_dwell) %>%
  scale()

# perform k-means clustering with k=3
set.seed(123) # for reproducibility
kmeans_fit <- kmeans(data_scaled, 3, nstart = 25)


# add cluster labels to dataset
data_clustered <- data %>% 
  mutate(cluster = kmeans_fit$cluster) %>%
  inner_join(raw,by="placekey")

# visualize clusters on map
ggplot(data_clustered, aes(x = raw_visitor_counts, y = median_dwell, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()

ggplot(data_clustered, aes(x = raw_visitor_counts, y = distance_from_home, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()
