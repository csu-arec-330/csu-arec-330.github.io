# This is the scipt for lab 05.

#Load necessary libraries
library(readr)
library(dplyr)

#epoch
as.numeric(as.Date("1970-01-01"))

#add one day to see how dates are tracked
as.numeric(as.Date("1970-01-02"))

#Read in the data
egg_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_01/inputs/APU0000708111.csv")

#Confirm that R read in the date field as a date
glimpse(egg_raw)

#Read in the data
egg_raw_c <- read_csv("https://csu-arec-330.github.io/materials/unit_01/inputs/APU0000708111_cdate.csv")

#Confirm that R read in the date field as text and *not* a date
glimpse(egg_raw_c)

# The easiest way to get lubridate is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just lubridate:
install.packages("lubridate")

#Load the necessary libraries
library(lubridate)

#Use the mutate() function along with the mdy() function to convert the text date to a date value
egg_raw_c <- mutate(egg_raw_c,measure_date=mdy(CHAR_DATE))

#Confirm that you have successfully converted the variable to a date field
glimpse(egg_raw_c)

egg_raw_c %>%
  mutate(measure_date=mdy(CHAR_DATE)) %>%
  glimpse()

#Install the package if you have not already done so. Comment this out after you have installed it once on your machine.
install.packages("tidyquant")

#Load the library. The library needs to be loaded every time you begin a new instance of R.
library(tidyquant, quietly = TRUE) #Note: The argument quietly = T in the library() function suppresses messages that would normally be printed when loading a package.

#Use the tq_get() function to retrieve the time series data
egg_raw_tq <- tq_get("APU0000708111",
                     get = "economic.data",
                     from="1990-01-01",
                     to="2024-02-28")

glimpse(egg_raw_tq)

#Use the tq_get() function to retrieve two time series datasets
egg_raw_tq <- tq_get(c("APU0000708111","WPU017107"),
                     get = "economic.data",
                     from="1990-01-01",
                     to="2024-02-28")

glimpse(egg_raw_tq)

#Display the unique values of the variable symbol
unique(egg_raw_tq$symbol)

#Count the number of observations associated with each value of symbol
table(egg_raw_tq$symbol)

#Use the FRED API to retrieve two data series and rename them the series with a more descriptive name
egg_out <- egg_raw_tq %>%
  mutate(description=case_when(
    symbol == "APU0000708111" ~ "Egg Retail Price", #if symbol equals APU0000708111, then replace value with Egg Retail Price
    symbol == "WPU017107" ~ "Egg Producer Price" #if symbol equals WPU017107, then replace value with Egg Producer Price
  ))

#Write the dataset to a csv file
write_csv(egg_out, "egg_out.csv")
