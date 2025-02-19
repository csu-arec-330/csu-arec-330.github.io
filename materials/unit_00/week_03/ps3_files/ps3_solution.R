
setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_00/week_03")

### Load necessary libraries
library(dplyr)
library(readr)

# 1. Read in the dataset
supermarket_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales.csv")

# 2. Calculate total value of sale and verify tax
super_sales <- supermarket_raw %>%
  mutate(
    subtotal = unit_price * quantity,
    tax_verify = subtotal * 0.05
  )

# 3. Filter dataset for "Food and beverages"
food_sales <- super_sales %>%
  filter(product_line == "Food and beverages")

# 4. Select relevant columns
food_sales_selected <- food_sales %>%
  select(city, product_line, unit_price, quantity, total, rating)

# 5. Sort by quantity in descending order
food_sales_sorted <- food_sales_selected %>%
  arrange(desc(quantity))

# 6. Calculate median sales by payment type
median_sales <- super_sales %>%
  group_by(payment_type) %>%
  summarise(median_total = median(total, na.rm = TRUE))

# 7. Compute rating per unit price (rup) and explain its insights
super_sales_rup <- super_sales %>%
  mutate(rup = rating / unit_price)

# 8. Summarize rup and unit_price by product line
summary_stats <- super_sales_rup %>%
  group_by(product_line) %>%
  summarise(
    mean_rup = mean(rup, na.rm = TRUE),
    mean_unit_price = mean(unit_price, na.rm = TRUE)
  )

# 9. Print summarized dataframe
print(summary_stats)

# 10. Display version and loaded packages
version
print(.packages())
