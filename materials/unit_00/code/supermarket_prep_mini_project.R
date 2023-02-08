#This script modifies the raw supermarket_sales data to introduce errors that need to be corrected in data processing

library(pacman)
p_load(readr,dplyr,janitor,tidyverse)

#Read in data
supermarket_raw <- read_csv("materials/unit_00/inputs/supermarket_sales.csv") %>%
  clean_names()

set.seed(20)

supermarket_mod <- supermarket_raw %>%
  select(-c(tax_5_percent,total,gross_margin_percentage,gross_income)) %>%  #First, remove the total and tax field so students need to calculate them
  mutate(cogs=cogs+rnorm(nrow(.)))  #Second, add noise to the cogs so that the gross margin percentage is not constant


write_csv(supermarket_mod,"unit_00/cache/supermarket_mod.csv")


######################################
# I will create a synthetic dataset with
#several features that will help the students "find" insights to report for
#their mini project.


# Make cogs vary so that gross margin varies across some dimension
# New years of data (pre-covid): 
# 1. sales growth in only one branch and maybe 1 or 2 product lines


# Import libraries
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(21)

n_obs <- 5000

# Generate synthetic data
invoice_id <- paste0(sample(100:999, n_obs, replace = TRUE), "-",
                     sample(10:99, n_obs, replace = TRUE), "-",
                     sample(1000:9999, n_obs, replace = TRUE))

city <- sample(c("Yangon", "Naypyitaw", "Mandalay"), n_obs, replace = TRUE)

customer_type <- sample(c("Member", "Normal"), n_obs, replace = TRUE)

gender <- sample(c("Male", "Female"), n_obs, replace = TRUE)

product_line <- sample(c("Health and beauty", "Electronic accessories", "Home and lifestyle","Fashion accessories","Food and beverages","Sports and travel"), n_obs, replace = TRUE)

unit_price <- round(runif(n_obs, 10, 100), 2)

quantity <- sample(1:20, n_obs, replace = TRUE)

measure_date <- sample(seq(as.Date("2017-01-01"), as.Date("2019-12-31"), by = "day"), n_obs, replace = TRUE)

measure_time <- format(sample(seq(as.POSIXct("2019-01-01 10:00:00"), as.POSIXct("2019-01-01 19:59:59"), by = "hour"), n_obs, replace = TRUE),format = "%H:%M:%S")

payment <- sample(c("Ewallet", "Cash", "Credit card"), n_obs, replace = TRUE)

#gross_margin_percentage <- round(runif(100, 1, 10), 2)

rating <- round(runif(n_obs, 4, 10), 1)

# Combine data into a dataframe
df <- data.frame(invoice_id, city, customer_type, gender, product_line, 
                 unit_price, quantity, measure_date, measure_time, 
                 payment, rating) %>%
  mutate(
    branch=case_when(
      city=="Yangon" ~ "A",
      city=="Mandalay" ~ "B",
      city=="Naypyitaw" ~ "C"),
    quantity=floor(ifelse((city=="Yangon" & product_line=="Electronic accessories" & dplyr::between(measure_date,as_date("2019-08-01"),as_date("2019-12-31"))),quantity*0.2,quantity)),
    quantity=floor(ifelse((city=="Yangon" & product_line=="Electronic accessories" & !dplyr::between(measure_date,as_date("2019-08-01"),as_date("2019-12-31"))),quantity*2.5,quantity)),
    rating=round(ifelse((city=="Yangon" & product_line=="Electronic accessories" & dplyr::between(measure_date,as_date("2019-05-01"),as_date("2019-12-31"))),rating*0.2,rating),1),
    subtotal=round(unit_price * quantity, 4),
    tax_5_percent=round(subtotal * 0.05, 4),
    total=round(subtotal + tax_5_percent, 4),
    cogs=round(subtotal * runif(n_obs, 0.5, 0.9), 2),
    gross_income=round(subtotal - cogs, 2),
    gross_margin_percentage=round(gross_income/subtotal*100,2),
    gross_income=floor(ifelse((payment=="Ewallet"),NA,gross_income)),
    gross_margin_percentage=floor(ifelse((payment=="Ewallet"),NA,gross_margin_percentage)),
    ) %>%
  select(-subtotal)

#GGally::ggpairs(select(df,where(is.numeric)))

# Write data to a csv file
#write_csv(df, "materials/unit_00/inputs/synthetic_data.csv")

# Write to 3 data files
df %>%
  group_split(year=year(measure_date)) %>%
  map(function(x){
    yr=unique(x$year)
    x %>%
      select(-year) %>%
      write_csv(.,str_c("materials/unit_00/inputs/supermarket_sales_",yr,".csv"))
  })

df %>%
  #group_by()
  ggplot(aes(x=measure_date,y=quantity)) +
  geom_point(alpha=.3) +
  geom_smooth() +
  facet_wrap(~product_line + city)
