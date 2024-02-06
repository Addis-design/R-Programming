#Preamble
## Name: <include your full name>
## Assignment: Lab 2
## Date: <here you may want to add a date>
## Purpose: <insert the goals or purpose of the RScript>
#Load the libraries
library(dplyr)
library(descr)
library(forcats)
library(ggplot2)
#view the data
?gss_cat
gss_cat
# research question: how marital status relate to the income?

#Part 2
#Data Cleaning and Manipulation
# Select the two columns
gss_subset <- gss_cat %>% select(marital, rincome)
# Remove rows with missing values
gss_subset <- na.omit(gss_subset)
gss_subset
#convert data to numerical
gss_subset$rincome <- as.numeric(gss_subset$rincome)
#explore the data
summary(gss_subset)

#summary statistics
summary_stats <- gss_subset %>%
  group_by(marital) %>%
  summarize(mean_income = mean(rincome), median_income = median(rincome))
#show summary statistics
summary_stats

# Create a boxplot with an adjusted title
ggplot(gss_subset, aes(x = marital, y = rincome)) +
  geom_boxplot() +
  labs(
    title = "Income Distribution by Marital Status",
    x = "Marital Status",
    y = "Income"
  ) +
  theme(plot.title = element_text(hjust =1))



# Create a bar plot
ggplot(gss_subset, aes(x = marital, y = rincome)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(
    title = "Average Income by Marital Status",
    x = "Marital Status",
    y = "Average Income"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Part 1 Reports
#Report 1.1
#What data set have you decided to use? gss_cat

#Report 1.2
#Which two variables from your data set will be analyzed? #marital vs rincome

#Report 1.3
#What is your research question? 
#how marital status relate to the level of income?

#Report 1.4
#What is your data analysis plan? Please be descriptive.
#Load the "gss_cat" dataset ,Check for missing values and outliers in the "rincome" variable.
#Calculate descriptive statistics for the "rincome" variable, such as mean, median, and standard deviation.
#Create visualizations, such as histograms, box plots, or bar plots, to understand the distribution of income by marital status.
#Interpret the results in the context of your research question, explaining the implications of the findings.
#Document the findings in a structured report or presentation.
#Use visual summaries and tables to highlight key results and insights.
#What are some potential limitations for your analysis?
#Causation vs. correlation The analysi show a correlation between marital status and income but cannot prove causation


#Part 2 Reports
#Report 1.6
#Does your data contain missing values? If so, how have you dealt with these values? Yes
any(is.na(gss_cat))

#Report 1.7
#Please include all code used to clean and manipulate the variables.
any(is.na(gss_cat))
#Data Cleaning and Manipulation
# Select the two columns
gss_subset <- gss_cat %>% select(marital, rincome)
# Remove rows with missing values
gss_subset <- na.omit(gss_subset)
gss_subset
#convert data to numerical
gss_subset$rincome <- as.numeric(gss_subset$rincome)
#explore the data

#Report 1.8
#What relationship, if any, exists between the two variables?
#"Widowed" have a higher average income compared to the other marital status categories.
#This observation is based on the "Mean" income and the visualizations.
#Report 1.9
#How do these findings relate to your research question and theory?
#The findings are directly related to my research question,
#as they provide initial insights into how different marital status categories are associated with income levels.  
# "Widowed" have a higher average income while the no answer group has the lowest average income

#Report 1.10
#What limitations exist as a result of the data analysis?
#The presence of missing values in the dataset limited the scope of may have introduced bias if not handled appropriately.
#The analysis show a correlation between marital status and income, but it does not prove causation.
  
  
