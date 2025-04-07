# Create shopper demographic file

setwd("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/csu-arec-330.github.io/materials/unit_02/week_01")
getwd() # Confirm I am working in the proper directory.

# Load required libraries using pacman for convenience
# pacman will check if the package is installed, install it if not, and then load it for use
#install.packages("pacman")
library(pacman)
p_load(dplyr,readr,tidyverse,ggplot2,modelsummary,GGally,factoextra,pandoc)


shopper_zip <- clean_data %>%
  select(shopper_id, zip_code) %>%
  distinct() %>%
  filter(!is.na(zip_code)) %>%         # remove NAs
  group_by(shopper_id, zip_code) %>%
  summarise(
    count = n(), 
    .groups = "drop"
  ) %>%
  group_by(shopper_id) %>%
  filter(count == max(count)) %>%      # keep the most frequent zip_code
  slice(1) %>%                         # if there's a tie, just take the first one
  ungroup() %>%
  select(shopper_id, zip_code) %>%        # drop count column
  mutate(shopper_zip = substr(zip_code, 1, 5)) %>%
  arrange(shopper_zip) %>%
  select(-zip_code)

write_csv(shopper_zip, "../inputs/shopper_zip.csv")

