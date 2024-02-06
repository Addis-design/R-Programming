#Preamble
## Name: Ray Will
## Lab 3
## Date:November 7,2023
## Purpose: Lab 3
#Load the libraries
library(dplyr)
library(descr)
library(forcats)
library(kableExtra)

#remotes::install_github("kjhealy/gssr")

#Installation using drat 
#if (!require("drat")) {
 # install.packages("drat")
 # library("drat")
##drat::addRepo("kjhealy")
#install.packages("gssr")
library(gssr)

#view the data
?gssr
gssr=gss18 <- gss_get_yr(2018)
help(gssr)
#Data Exploration:

str(gssr)
summary(gssr)
head(gssr)
table(gssr)
#we Explore two variables: age vs. rincome
gssr$rincome
#Data Cleaning and Manipulation
# Select the two columns
gss_subset <- gssr %>% select(age, rincome)
# Remove rows with missing values
gss_subset <- na.omit(gss_subset)
table(gss_subset)
#check for missing values
sum(is.na(gss_subset$rincome))
sum(is.na(gss_subset$age))
#convert data to numerical
gss_subset$rincome <- as.numeric(gss_subset$rincome)
#explore the data
summary(gss_subset)

#summary statistics
summary_stats <- gss_subset %>%
  group_by(age) %>%
  summarize(mean_income = mean(rincome), median_income = median(rincome))
#show summary statistics
summary_stats

# Create Plots
# Melt the data for easy plotting
summary_stats_melted <- tidyr::pivot_longer(summary_stats, cols = c(mean_income, median_income), names_to = "Income_Type", values_to = "Income_Value")


# Sort the data by Age for better visualization
summary_stats_melted <- summary_stats_melted[order(summary_stats_melted$age),]

# Plotting a line graph
# Sort the data by Age for better visualization
summary_stats_melted <- summary_stats_melted[order(summary_stats_melted$age),]

# Unique Income Types
unique_income_types <- unique(summary_stats_melted$Income_Type)

# Create an empty plot to set the range and labels
plot(summary_stats_melted$age, summary_stats_melted$Income_Value, type = "n", xlab = "Age", ylab = "Income", main = "Relationship between Age and Income")

# Loop through each unique Income_Type and plot each line separately with a distinct color
for (i in 1:length(unique_income_types)) {
  subset_data <- subset(summary_stats_melted, Income_Type == unique_income_types[i])
  lines(subset_data$age, subset_data$Income_Value, type = "l", col = rainbow(length(unique_income_types))[i])
}

# Add a legend
legend("topright", legend = unique_income_types, col = rainbow(length(unique_income_types)), lty = 1)

# Create boxplot
boxplot(rincome ~ as.factor(age), data = gss_subset, xlab = "Age Group", ylab = "Income",
        main = "Income Distribution by Age Group")


# Create a bar plot showing average income by age
mean_income <- tapply(gss_subset$rincome, gss_subset$age, mean)

barplot(mean_income, names.arg = unique(gss_subset$age), xlab = "Age", ylab = "Average Income",
        main = "Average Income by Age", col = rainbow(length(mean_income)))

#Regression Analysis
#Perform linear regression
lm_model <- lm(rincome ~ as.factor(age), data = gss_subset)

# Summary of the regression model
summary(lm_model)

#visualize the Results

# Residuals vs Fitted plot
plot(lm_model$fitted.values, lm_model$residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, lty = 2, col = "red")  # Add a dashed line at y = 0

# Q-Q plot
qqnorm(rstandard(lm_model))
qqline(rstandard(lm_model))
title("Normal Q-Q Plot")

# Scale-Location plot (Square root of standardized residuals vs fitted values)
plot(lm_model$fitted.values, sqrt(abs(rstandard(lm_model))), xlab = "Fitted values", ylab = "Square root of standardized residuals", main = "Scale-Location Plot")
lines(lowess(lm_model$fitted.values, sqrt(abs(rstandard(lm_model)))), col = "blue")

# Cook's distance plot
plot(cooks.distance(lm_model), type = "l", xlab = "Observation number", ylab = "Cook's distance", main = "Cook's Distance Plot")



# Categorizing ages into groups
gssr <- gssr %>%
  mutate(age_group = cut(age, breaks = seq(18, 90, by = 5), right = FALSE))

# Scatter plot of Age vs. Income
plot(gssr$age, gssr$income, xlab = "Age", ylab = "Income", main = "Relationship between Age and Income")

