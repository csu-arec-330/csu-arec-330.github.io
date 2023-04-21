# This script demonstrates how to estimate a difference in differences model using the did package

# load packages
library(pacman)
p_load(tidyverse,did,fixest,modelsummary,tsibble)

# call in dataset
data("mpdta")

#look at the data
glimpse(mpdta) #or View(mpdta)

#Create treated variable and convert year and county to factors
mpdta_new <- mpdta %>%
  mutate(treated=ifelse(year>=first.treat & treat==1,1,0),
         year=as.factor(year),
         countyreal=as.factor(countyreal))

#Estimate model with dummy variables
m1 <- lm(lemp ~ treated + countyreal + year,data = mpdta_new)
summary(m1)

# generate regression table
modelsummary(m1,coef_map = "treated")

#Alternative way to estimate fixed effects regression
felm <- feols(lemp ~ treated | countyreal + year,data = mpdta_new)
summary(felm)

##################################
#How does fuel moisture impact the number of fire days in a county?

#Read the fire and weather dataset
fires <- read_csv("sandbox/fires_303.csv.gz")
weather <- read_csv("sandbox/weather_303.csv.gz")

#Join data and aggregate over year-month
analysis_ds <- inner_join(fires,
                          weather,
                          by=c("county_fips"="county","measure_date")) %>%
  group_by(county_fips,ym=yearmonth(measure_date)) %>%
  summarize(fires=sum(fires),
            fm1000=mean(fm1000)) %>%
  ungroup()

#Estimate two-way fixed effects regression
fire_model <- feols(fires ~ fm1000 | county_fips + ym,
                    data=analysis_ds)

summary(fire_model)
etable(fire_model)



