#load libraries
library(readxl)
library(ggplot2)
corolla_data <- read_excel("Tab 4.xlsx")
head(corolla_data)
summary(corolla_data)
 

# Histogram of Prices
ggplot(corolla_data, aes(x = Price)) +
  geom_histogram(binwidth = 500) +
  labs(title = "Histogram of Prices", x = "Price", y = "Frequency")


#Histogram of Prices colored by Fuel Type
hist_price_fuel <- ggplot(corolla_data, aes(x = Price, fill = Fuel_Type)) +
  geom_histogram(binwidth = 500, color = "black", aes(y = ..count..)) +
  labs(title = "Histogram of Prices Colored by Fuel Type", x = "Price", y = "Count", fill = "Fuel Type") +
  theme_minimal()
# Print the plot
print(hist_price_fuel)


# Scatter Plot of Prices vs. Age
scatter_price_age <- ggplot(corolla_data, aes(x = Age_08_04, y = Price)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = "Scatter Plot of Prices vs. Age", x = "Age", y = "Price") +
  theme_minimal()
scatter_price_age

# Scatter Plot of Prices vs Age colored by Airco
scatter_price_age_airco <- ggplot(corolla_data, aes(x = Age_08_04, y = Price, color = Airco)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of Prices vs. Age Colored by Airco",
       x = "Age", y = "Price", color = "Airco") +
  theme_minimal()
# Print the plot
print(scatter_price_age_airco)


# Box Plot of Prices segregated by Mfg Year
boxplot_price_mfg_year <- ggplot(corolla_data, aes(x = as.factor(Mfg_Year), y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot of Prices Segregated by Mfg Year",
       x = "Manufacturing Year", y = "Price") +
  theme_minimal()
#print plot
print(boxplot_price_mfg_year)


# Save the plots
ggsave("hist_price.png", hist_price, width = 8, height = 6)
ggsave("hist_price_fuel.png", hist_price_fuel, width = 8, height = 6)
ggsave("scatter_price_age.png", scatter_price_age, width = 8, height = 6)
ggsave("scatter_price_age_airco.png", scatter_price_age_airco, width = 8, height = 6)
ggsave("boxplot_price_mfg_year.png", boxplot_price_mfg_year, width = 8, height = 6)
