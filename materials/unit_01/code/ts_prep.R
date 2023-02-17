#This script reads in and processes data from FRED

library(readr)
library(dplyr)
library(lubridate)

###
egg_raw <- read_csv("materials/unit_01/inputs/APU0000708111.csv")

#Change format of date
egg_raw %>%
  mutate(CHAR_DATE=format(DATE,"%m/%d/%Y")) %>%
  select(-DATE) %>%
  write_csv("materials/unit_01/inputs/APU0000708111_cdate.csv")


egg_raw_c <- read_csv("https://csu-arec-330.github.io/materials/unit_01/inputs/APU0000708111_cdate.csv")


library(tidyquant)


tq_get(c("APU0000708111"),get = "economic.data",from="1990-01-01")

#WPU017107: Producer Price Index by Commodity: Farm Products: Eggs for Fresh Use
#WPU012: Producer Price Index by Commodity: Farm Products: Grains
#WPU023: Producer Price Index by Commodity: Processed Foods and Feeds: Dairy Products 
fred_data <- tq_get(c("WPU017107","WPU012","WPU023"),get = "economic.data",from="1900-01-01")
