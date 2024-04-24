
library(pacman)

# Load ggplot2 package
p_load(tidyverse,modelsummary)

len=150

# Set seed for reproducibility
set.seed(20)

# Generate data
hours_studied <- rnorm(len,mean = 25,sd=18) #1:len  # Students studying between 1 to 50 hours
hours_studied <- hours_studied[between(hours_studied,0,100)]
#hours_studied <- ifelse(hours_studied>50,50,hours_studied)
test_scores <- 40 + .8 * hours_studied + rnorm(length(hours_studied), mean = 0, sd = 15)  # Basic linear model with noise




# Combine into a data frame
data <- data.frame(hours_studied, test_scores) %>%
  filter(between(test_scores,0,100))

ggplot(data, aes(x = hours_studied, y = test_scores)) +
  geom_point(alpha=.6) +  # Add points for scatter plot
  geom_smooth(method = "lm", se = FALSE, col = "blue") +  # Add linear regression line
  labs(subtitle = "Relationship between Hours Studied and Test Scores",
       x = "Hours Studied",
       y = "Test Scores") +
  theme_bw(base_size = 15)

#Estimate the regression
model1 <- lm(test_scores ~ hours_studied,data)

m_out <- modelsummary(model1,stars = TRUE,coef_rename = c("(Intercept)"="Intercept","hours_studied"="Hours Studied"),gof_map = c("nobs", "r.squared"))

m_out








