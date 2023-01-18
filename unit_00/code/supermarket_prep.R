#This script modifies the raw supermarket_sales data to introduce errors that need to be corrected in data processing

library(pacman)
p_load(readr,dplyr,janitor)

#Read in data
supermarket_raw <- read_csv("unit_00/inputs/supermarket_sales.csv") %>%
  clean_names()

set.seed(20)

supermarket_mod <- supermarket_raw %>%
  select(-c(tax_5_percent,total,gross_margin_percentage,gross_income)) %>%  #First, remove the total and tax field so students need to calculate them
  mutate(cogs=cogs+rnorm(nrow(.)))  #Second, add noise to the cogs so that the gross margin percentage is not constant


write_csv(supermarket_mod,"unit_00/cache/supermarket_mod.csv")
