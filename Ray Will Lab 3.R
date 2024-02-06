#Preamble
## Name: Ray Will
## Lab 3
## Date:November 7,2023
## Purpose: Lab 3
#Load the libraries
library(dplyr)
library(descr)
library(forcats)
library(ggplot2)
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
#gssr=gss18 <- gss_get_yr(2018)
help(gssr)
#Data Exploration:

str(gssr)
summary(gssr)
head(gssr)
#we Explore two variables: age vs. rincome
gssr$rincome
#Data Cleaning and Manipulation
# Select the two columns
gss_subset <- gssr %>% select(age, rincome)
# Remove rows with missing values
gss_subset <- na.omit(gss_subset)
gss_subset
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

# Plot the data
ggplot(summary_stats_melted, aes(x = age, y = Income_Value, color = Income_Type)) +
  geom_line() +
  labs(title = "Relationship between Age and Income", x = "Age", y = "Income")


# Create a boxplot  
ggplot(gss_subset, aes(x = as.factor(age), y = rincome)) +
  geom_boxplot() +
  labs(
    title = "Income Distribution by Age Group",
    x = "Age Group",
    y = "Income"
  ) +
  theme(plot.title = element_text(hjust = 0.5))  # Adjust hjust to center the title


# Create a bar plot
ggplot(gss_subset, aes(x = as.factor(age), y = rincome, fill = as.factor(age))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(
    title = "Average Income by Age",
    x = "Age",
    y = "Average Income"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Regression Analysis
#Perform linear regression
lm_model <- lm(rincome ~ as.factor(age), data = gss_subset)

# Summary of the regression model
summary(lm_model)

#visualize the Results

# Residuals vs Fitted plot
plot_res_fitted <- ggplot(data = data.frame(residuals = lm_model$residuals, fitted = lm_model$fitted.values), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# Q-Q plot
plot_qq <- ggplot(data = data.frame(std_res = rstandard(lm_model)), aes(sample = std_res)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot")

# Scale-Location plot (Square root of standardized residuals vs fitted values)
plot_scale_loc <- ggplot(data = data.frame(sqrt_std_res = sqrt(abs(rstandard(lm_model))), fitted = lm_model$fitted.values), aes(x = fitted, y = sqrt_std_res)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Scale-Location Plot", x = "Fitted values", y = "Square root of standardized residuals")

# Cook's distance plot
plot_cooks <- ggplot(data = data.frame(cooks = cooks.distance(lm_model)), aes(x = seq_along(cooks), y = cooks)) +
  geom_line() +
  labs(title = "Cook's Distance Plot", x = "Observation number", y = "Cook's distance")

# Arranging the plots
library(gridExtra)
grid.arrange(plot_res_fitted, plot_qq, plot_scale_loc, plot_cooks, nrow = 2)

