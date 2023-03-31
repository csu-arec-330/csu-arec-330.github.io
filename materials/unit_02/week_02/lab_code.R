#clustering example

# load packages
library(pacman)
p_load(tidyverse,ggplot2,GGally,broom,ranger,rsample,caret)

# read in dataset
raw <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/arizona_grocery_foot_traffic.csv")

location_info <- raw %>%
  select(placekey,location_name,top_category,sub_category,latitude,longitude,city,region)
############################
#EDA

#Summary statistics

sumstats <- skimr::skim(raw)
ss

ggpairs(raw,columns = c("raw_visitor_counts","distance_from_home","median_dwell")) 

raw %>%
  select(raw_visitor_counts,distance_from_home,median_dwell) %>%
  mutate(across(everything(),log)) %>%
  ggpairs()

#You may want to remove very large outliers: long dwells are probably employees, long distances are probably tourists
raw %>%
  select(raw_visitor_counts,distance_from_home,median_dwell) %>%
  mutate(across(everything(),log)) %>%
  filter() %>%
  ggpairs()

###########################

# select relevant columns and scrub outliers
data <- raw %>% 
  dplyr::filter(top_category=="Grocery Stores", #focus on those classified as grocery stores
                distance_from_home<48000,median_dwell<90) %>%
  select(placekey,raw_visitor_counts,distance_from_home,median_dwell) %>%
  mutate(across(c(2:4),log)) %>%
  drop_na()



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
  inner_join(location_info,by="placekey")

# visualize clusters on map
ggplot(data_clustered, aes(x = raw_visitor_counts, y = median_dwell, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()

ggplot(data_clustered, aes(x = raw_visitor_counts, y = distance_from_home, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()

ggplot(data_clustered, aes(x = median_dwell, y = distance_from_home, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()

ggplot(data_clustered, aes(x = longitude, y = latitude, color = factor(cluster))) + 
  geom_point(alpha=.3) + 
  theme_minimal()

ggplot(data_clustered,aes(y=sub_category,x=factor(cluster),fill=factor(sub_category))) +
  geom_col()

###########################

m1 <- lm(raw_visitor_counts ~ distance_from_home + median_dwell,data = data_clustered)

summary(m1)

summary(data_clustered)

new_data <- tibble(distance_from_home=seq(5,11,.01),
                   median_dwell=seq(1,7,.01))

fitted_data <- augment(m1,newdata = new_data) %>%
  rename(raw_visitor_counts=.fitted)

ggplot(fitted_data,aes(x=distance_from_home,y=raw_visitor_counts)) +
  geom_point()

write_csv(fitted_data,"fitted_reg.csv")

##########################
#Classification


#Divide data into training and testing sample
set.seed(123)
data_split <- initial_split(data_clustered,prop=.7)
train_data <- training(data_split)
test_data <- testing(data_split)

#Fit the random forest model
rf_model <- ranger(factor(sub_category) ~ raw_visitor_counts + distance_from_home + median_dwell,
                   data = train_data,
                   num.trees = 500)



rf_predict <- predict(rf_model,data = test_data)

cm <- caret::confusionMatrix(rf_predict$predictions,factor(test_data$sub_category))

print(cm)

all_predict <- predict(rf_model,data = data_clustered)

output_data <- data_clustered %>%
  mutate(pred_sub_category = all_predict$predictions)

write_csv(output_data,"analyzed_data.csv")