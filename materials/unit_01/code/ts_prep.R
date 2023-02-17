#This script reads in and processes data from FRED

library(readr)
library(dplyr)
library(lubridate)

###
egg_raw <- read_csv("materials/unit_01/inputs/WPU017107.csv")

#Change format of date
egg_raw %>%
  mutate(CHAR_DATE=format(DATE,"%m/%d/%Y")) %>%
  select(-DATE) %>%
  write_csv("materials/unit_01/inputs/WPU017107_cdate.csv")

library(tidyquant)


tq_get(c("WPU017107","WPU012"),get = "economic.data",from="1990-01-01")

tq_get(c("WPU017107","WPU012"),get = "economic.data",from="1990-01-01")
