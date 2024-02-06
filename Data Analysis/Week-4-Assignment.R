#load libraries
library("ggplot2")

#read the dataset
data<-read.csv('shopping_behavior_updated.csv')
#view the dataset
head(data)
# Create a contingency table for Gender and Subscription Status
contingency_table <- table(data$Gender, data$Subscription.Status)

# Calculate chi-square test
chi_square_result <- chisq.test(contingency_table)

# Print the contingency table
print(contingency_table)

# Print chi-square test results
print(chi_square_result)

# Calculate the correlation coefficient between Age and Purchase Amount
correlation_coefficient <- cor(data$Age, data$Purchase.Amount..USD.)

# Create a scatter plot
ggplot(data, aes(x = Age, y = Purchase.Amount..USD.)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Age and Purchase Amount",
       x = "Age",
       y = "Purchase Amount") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, color = "red")  # Add a linear regression line for better insight

# Print the correlation coefficient
print(paste("Correlation Coefficient:", correlation_coefficient))
