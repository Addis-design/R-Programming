#Question 2) Problem Set 3
# Create a vector of random numbers
my_vec <- runif(100, min = 0, max = 100)

# Check the structure and summary of my_vec
str(my_vec)
summary(my_vec)
   
# Plot a histogram
hist(my_vec)

# Load the mtcars dataset
data(mtcars)

# Check class, structure, and summary of mtcars
class(mtcars)
str(mtcars)
summary(mtcars)

"""
Financial Analysis: Consider financial data, such as stock market prices, economic indicators, or company financial reports, to perform financial modeling, risk analysis, or investment strategies.

"""


#First Dynamic Report
iris
# Get the column names and data types
colnames(iris)
sapply(iris, class)

# Get the dimensions (rows and columns) of the Iris dataset
dim(iris)
# Create a vector 'width' from the 'Sepal.Width' column of the Iris dataset
width <- iris$Sepal.Width

# What is the 100th value in the 'width' vector?
value_100 <- width[100]

# What is the last value in the 'width' vector?
last_value <- tail(width, n = 1)

# Select rows 10 to 20 with all columns in the Iris dataset
subset_iris <- iris[10:20, ]

# Output the results
value_100
last_value
subset_iris
#Select rows 10 to 20 with only the Species, Petal.Width, and Petal.Length:
subset_1 <- iris[10:20, c("Species", "Petal.Width", "Petal.Length")]
subset_2 <- iris[10:20, c(5, 4, 3)]
#Select rows 1 to 10, 20, and 100 in the iris dataset
subset_rows <- iris[c(1:10, 20, 100), ]
#Select the first value in the Sepal.Length column of the iris dataset
iris[1, "Sepal.Length"]
iris$Sepal.Length[1]
head(iris$Sepal.Length, n = 1)
#Selecting the First Value in the Sepal.Length Column (three different ways)
iris$Sepal.Length[1]
iris[1, "Sepal.Length"]
iris[1, ]$Sepal.Length

#Without running the following code in R, try to determine which of the following will return the ﬁrst three rows of the Sepal.Length column in the iris data.frame? For each of the answers that do not work, see if you explain why!
