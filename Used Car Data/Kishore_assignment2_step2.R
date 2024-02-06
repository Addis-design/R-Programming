#Importing and saving data from CSV files
usedcars<-read.csv('Desktop/Projects/R/Used Car Data/DealerShipUsedCars.csv', stringsAsFactors = FALSE)
write.csv(usedcars, file = "usedcars.csv", row.names = FALSE)
usedcars<-read.csv('usedcars.csv', stringsAsFactors = FALSE)
str(usedcars)
# 1. Show all numerical variables in the dataset
summary(usedcars$Year)
summary(usedcars[c('Price','Mileage')])
# 2. Show the price range of the cars and the difference
#range
range(usedcars$Price)
#difference
diff(range(usedcars$Price))
# 3. Using one command to show the number of cars with the same make
table(usedcars$Make)
# 4. Using the prop.table() function to show the proportion of each car's make of the whole
make_counts<-table(usedcars$Make)
car_make=prop.table(make_counts)
car_make
car_make=car_make*100
round(car_make,digits = 2)

# The results show that HONDA and JEEP are the most common car makes, since nearly a quarter (33.12 percent) of all the advertised cars are  HONDA and JEEP . TOYOTA is a close second with 22.5 percent and FORDis third with 8.12 percent


#5. Show which car’s color is the highest present in an inventory.
color_cr<-table(usedcars$Color)
color_cr=prop.table(color_cr)*100
round(color_cr,digits = 2)
most_common_color <- names(sort(table(usedcars$Color), decreasing = TRUE))[1]
cat("Most Common Color:", most_common_color, "\n")

#The results show that Black is the most common color, since nearly a quarter (23.12 percent) of all the advertised cars are Black. Silver is a close second with 21.25 percent and Red is third with 20.00 percent

## 6. Using CrossTable function to show how many cars from each Make have the color (Red, Yellow, Black, Gray, White, Silver)
#install.packages('gmodels')
library(gmodels)
usedcars$conservative<-usedcars$Color %in% c('Red','Yellow','Black','Gray','White','Silver')
table(usedcars$conservative)
CrossTable(x=usedcars$Make,y=usedcars$conservative)
cross_table <- CrossTable(usedcars$Make, usedcars$conservative)
print(cross_table)

# 7. Analyze the relationship between price and year using a scatterplot
plot(usedcars$Year, usedcars$Price, xlab = "Used Car Year", ylab = "Used Cars Price ($)", main = "Scatterplot of Price vs. Year")

# Calculate and display correlation coefficient to assess the relationship
correlation_coefficient <- cor(usedcars$Year, usedcars$Price)
cat("Correlation Coefficient (Price vs. Year):", correlation_coefficient, "\n")


#Yes, there is a relation between the price and the year of the cars. As the year of the car increases (i.e., the cars are newer), the price tends to be higher. On average, newer cars are more expensive.






