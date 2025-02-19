
setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_00/week_04/ps4_files")
getwd()

library(readr)
library(dplyr)
library(tidyr)

super_2017 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2017.csv")
super_2018 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2018.csv")
super_2019 <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales_2019.csv")

super_sales_all <- bind_rows(super_2017, super_2018, super_2019)


# Option 1: Calculate gross_income and gross_margin_percentage using the available data <<< This is what the problem set asked for >>>
super_sales_clean <- super_sales_all %>%
  mutate(gross_income = if_else(is.na(gross_income), total - tax - cogs, gross_income)) %>%
  mutate(gross_margin_percentage = if_else(is.na(gross_margin_percentage), (gross_income/total)*100, gross_margin_percentage)) %>%
  mutate(gross_margin_percentage = if_else(is.nan(gross_margin_percentage), 0, gross_margin_percentage))


# Option 2: Use mutate_all() to apply replace_na() to all columns. Automatically fills numeric columns with 0 and character columns with "Unknown". <<< I would also accept this - this is what we did in lab >>>
super_sales_clean <- super_sales_all %>%
  mutate_all(~replace_na(.x, ifelse(is.numeric(.x), 0, "Unknown")))


# Check for missing values after filling
colSums(is.na(super_sales_clean))


write_csv(super_sales_clean,"super_sales_clean.csv")
