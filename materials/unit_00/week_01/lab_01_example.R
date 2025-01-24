# Lab Script for Week 1: Introduction to R

# Set and check the working directory
getwd() # Displays the current working directory
setwd("C:/Users/YourName/Documents/MyRProjects") # Sets the working directory

# Assign values to variables
x <- 5 # Assign 5 to x using the preferred assignment operator
x = 5 # Does the same thing but using = for assignment

# Print the value of x
print(x)

# Perform basic arithmetic
# Compute 4 times 5
4 * 5

y <- 3 # Assign 3 to y
z <- x + y # Add x and y, assign the result to z
print(z) # Print the value of z

# Divide two variables
a <- 30 # Assign 30 to a
b <- 10 # Assign 10 to b
a / b # Divide a by b

# Create vectors of different data types
numbers <- c(1, 2, 3, 4, 5) # Numeric vector
characters <- c("a", "b", "c") # Character vector
logical <- c(TRUE, FALSE, TRUE) # Logical vector

# Access elements of a vector
print(numbers[1]) # Print the first element of numbers
print(numbers[3]) # Print the third element of numbers

print(characters[2:3]) # Print the second and third elements of characters
print(characters[c(1,3)]) # Print the first and third elements of characters

# Can you sum the numbers of the vector characters? How about logical?
# Attempting to sum the characters vector will result in an error because it contains non-numeric data.
# Uncomment the line below to see the error.
# sum(characters)

# Sum the logical vector. TRUE is treated as 1, and FALSE is treated as 0.
# print(sum(logical))

# Perform vectorized operations
print(numbers+5) # Add 5 to each element of numbers
print(numbers + numbers*2) # Multiply each element of numbers by 2 and add it to the original numbers

# Can you figure out how to calculate the product of the vector numbers?
# Use the prod() function to calculate the product of all elements in the vector
print(prod(numbers))

# Create and display a data frame
print(data.frame(a=c(1:3),
                 b=c(4:6),
                 d=c(7:9)))

# Create a data frame using existing vectors
df <- data.frame(numbers=numbers[1:3],
                 characters,
                 logical)

print(df) # Print the data frame

# Read in an external data set
super_sales <- read.csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales.csv")

head(super_sales) # Display the first few rows of the data set

# Install and load a package
# Run the following command only once to install the package
# install.packages("dplyr")
library(dplyr) # Load the dplyr package
