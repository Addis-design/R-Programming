# Load necessary libraries
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(skimr)

# Load the dataset
data <- read_excel("/home/addis/Downloads/alcohol.xlsx")

#Description of the data
summary_stats <- summary(data[, c("wgsal", "drnkmo", "age", "higrad", "sex")])

# Print the summary statistics
print(summary_stats)
 
# Assuming your dataset is named 'data'

# Scatterplot for Age vs Total Wage and Salary Income
ggplot(data, aes(x = age, y = wgsal)) +
  geom_point() +
  labs(title = "Scatterplot of Age vs Total Wage and Salary Income",
       x = "Age",
       y = "Total Wage and Salary Income")

# Compute and print detailed descriptive statistics
desc_stats <- skim(data[, c("wgsal", "drnkmo", "age", "higrad", "sex")])
print(desc_stats)
# To create a table of summary statistics
summary_table <- data.frame(
  Mean = sapply(data[, c("wgsal", "drnkmo", "age", "higrad")], mean),
  Median = sapply(data[, c("wgsal", "drnkmo", "age", "higrad")], median),
  SD = sapply(data[, c("wgsal", "drnkmo", "age", "higrad")], sd),
  Min = sapply(data[, c("wgsal", "drnkmo", "age", "higrad")], min),
  Max = sapply(data[, c("wgsal", "drnkmo", "age", "higrad")], max)
)

# Print the summary table
print(summary_table)

# View the structure of your dataset
str(data)

# Check for missing values
sum(is.na(data))

# Explore the first few rows of the dataset
head(data)

# Create a scatterplot
ggplot(data, aes(x = drnkmo, y = wgsal)) +
  geom_point() +
  labs(title = "Scatterplot of wgsal vs. drnkmo",
       x = "drnkmo (Had a drink in the last month)",
       y = "wgsal (Total wage and salary income)")


#d) Estimation Results
# Estimate the regression model

# Model 1: Total wage and salary income as the dependent variable
model1 <- lm(wgsal ~ drnkev + age + higrad + sex, data = data) 
# View the summary of the regression model
summary(model1)

# Model 2: Total number of hours worked as the dependent variable
model2 <- lm(hrswrk ~ drnkmo + age + higrad + sex, data = data)
# View the summary of the regression model

summary(model2)


# Model 3: Total number of weeks worked as the dependent variable
model3 <- lm(wkswrk ~ drnk6m + age + higrad + sex, data = data)
# View the summary of the regression model

summary(model3)

# Model 4: Employment status as the dependent variable
model4 <- lm(empst ~ perday + age + higrad + sex, data = data)
# View the summary of the regression model

summary(model4)
 
# Conduct hypothesis tests on coefficients
test_drinkev <- broom::tidy(model1, hypothesis = c("drinkev = 0"))
print(test_drinkev)

# Conduct hypothesis tests for Model 2
test_drnkmo <-  broom::tidy(model2, hypothesis = c("drnkmo = 0"))
print(test_drnkmo)

# Conduct hypothesis tests for Model 3
test_drnk6m <-  broom::tidy(model3, hypothesis = c("drnk6m = 0"))
print(test_drnk6m)

# Conduct hypothesis tests for Model 4
test_perday <-  broom::tidy(model4, hypothesis = c("perday = 0"))
print(test_perday)

# Construct 95% confidence intervals for the coefficients
# Model 1: Total wage and salary income as the dependent variable
conf_int_model1 <- confint(model1, level = 0.95)
print(conf_int_model1)

# Visualize the results for Model 1
par(mfrow=c(2,2))
plot(model1)

# Model 2: Total number of hours worked as the dependent variable
conf_int_model2 <- confint(model2, level = 0.95)
print(conf_int_model2)

# Visualize the results for Model 2
par(mfrow=c(2,2))
plot(model2)

# Model 3: Total number of weeks worked as the dependent variable
conf_int_model3 <- confint(model3, level = 0.95)
print(conf_int_model3)

# Visualize the results for Model 3
par(mfrow=c(2,2))
plot(model3)

# Model 4: Employment status as the dependent variable
conf_int_model4 <- confint(model4, level = 0.95)
print(conf_int_model4)

# Visualize the results for Model 4
par(mfrow=c(2,2))
plot(model4)

# Confidence intervals
# Function to print confidence intervals, number of observations, and R-squared
print_model_summary <- function(model) {
  # Confidence intervals
  conf_int <- confint(model, level = 0.95)
  print(conf_int)
  
  # Number of observations and R-squared
  num_obs <- nobs(model)
  r_squared <- summary(model)$r.squared
  
  # Print number of observations and R-squared
  cat("Number of Observations: ", num_obs, "\n")
  cat("R-squared: ", r_squared, "\n")
}

# Apply the function to each model
print_model_summary(model1)
print_model_summary(model2)
print_model_summary(model3)
print_model_summary(model4)
