# These are the lab notes for Week 04

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_00/week_04")

library(readr)
library(dplyr)
library(janitor)
library(tidyr)

# Load data
super_2017 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2017.csv")
super_2018 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2018.csv")
super_2019 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2019.csv")

# Append all data frames
super_sales_all <- bind_rows(super_2017, super_2018, super_2019)

# Check for missing data: Use the dplyr functions (mutate() and replace_na()) to fill missing values.
colSums(is.na(super_2017))

# Use mutate_all() to apply replace_na() to all columns. 
# What does this do? It automatically fills numeric columns with 0 and character columns with "Unknown". 
super_sales_clean <- super_sales_all %>%
  mutate_all(~replace_na(.x, ifelse(is.numeric(.x), 0, "Unknown")))

# Check for missing values after filling
colSums(is.na(super_sales_clean))


head(super_sales_clean)

# Export the data as a CSV
write_csv(super_sales_clean,"super_sales_clean.csv")

