# This script is a sandbox for messing with the supermarket data

#Preliminaries: set directory, check environment, load needed packages
#setwd("~")
ls()

# install.packages("pacman")
library(pacman)
p_load(readr,dplyr,janitor)

#read in data and column names
supermarket_raw <- read_csv("unit_00/cache/supermarket_mod.csv") %>%
  clean_names()

#Look at data both in viewer and in console
glimpse(supermarket_raw)

#Discuss data types



#Assignment 1: simple processing - filtering, selecting, transforming
#Modifying columns - mutate
supermarket_sales <- mutate(supermarket_raw,total=unit_price*quantity)

#Sorting


#Filtering
table(supermarket_raw$product_line)
supermarket_food <- filter(supermarket_raw,product_line=="Food and beverages")

#Selecting
supermarket_food_subset <- select(supermarket_food,
                                  invoice_id,
                                  branch,
                                  city,
                                  customer_type,
                                  gender,
                                  total,
                                  payment)

write_csv(supermarket_food_subset,"supermarket_food_subset.csv")



#Check data
super_checked <- supermarket_raw %>%
  mutate(total_check = unit_price*quantity + tax_5_percent)

glimpse(select(super_checked,total,total_check))

mutate(super_checked,gross_margin_check=gross_income/cogs*100) %>%
  select(gross_margin_check,gross_margin_percentage)

p_load(ggplot2)

supermarket_raw %>%
  ggplot(aes(gross_income,rating)) +
  geom_point() +
  facet_wrap(~product_line)
