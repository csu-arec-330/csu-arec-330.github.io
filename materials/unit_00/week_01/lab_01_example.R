x <- 5
y <- 3
z <- x + y
print(z)


numbers <- c(1, 2, 3, 4, 5)
characters <- c("a", "b", "c")
logical <- c(TRUE, FALSE, TRUE)

print(numbers[1])

print(characters[2:3])

print(characters[c(1,3)])

print(numbers+5)

print(data.frame(a=c(1:3),
                 b=c(4:6),
                 d=c(7:9)))

getwd()

super_sales <- read.csv("https://csu-arec-330.github.io/materials/unit_00/inputs/supermarket_sales.csv")

head(super_sales)
