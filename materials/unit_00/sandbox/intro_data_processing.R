# This script is a sandbox for messing with the supermarket data

#Preliminaries: set directory, check environment, load needed packages
#setwd("~")
ls()

# install.packages("pacman")
library(pacman)
p_load(readr,dplyr,janitor)

#read in data and column names
supermarket_raw <- read_csv("materials/unit_00/inputs/supermarket_sales.csv") %>%
  clean_names()

#Look at data both in viewer, in console, and in excel
glimpse(supermarket_raw)

#Discuss data types



#Assignment 1: simple processing - filtering, selecting, transforming
#Modifying columns - mutate
my_super_sales <- mutate(supermarket_raw,total_calc=unit_price*quantity)
glimpse(my_super_sales)

my_super_sales <- mutate(supermarket_raw,total_calc=unit_price*quantity*1.05,
                         check_tax=tax_5_percent/cogs)

glimpse(my_super_sales)

#Selecting
select_super_sales <- select(my_super_sales,invoice_id,city,total,total_calc)
glimpse(select_super_sales)


#Sorting
yangon_super_sales_sorted <- arrange(yangon_super_sales,total)
glimpse(yangon_super_sales_sorted)


super_sales_top <- arrange(select_super_sales,city,desc(total))
glimpse(super_sales_top)

