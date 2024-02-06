#Question 1

library(expm)
# Define the matrix Z
Z <- matrix(c(500, 320, -5, 350, 360, -64, 110, 20, 34), nrow = 3, byrow = TRUE)

# Define the vector of constants b
b <- matrix(c(1097, 453, 117), 3, byrow = T)

# a) Our system of equations is Zi + d = b
# Solve for vector d_a
d_a <- solve(Z, b)

# b) Our system of equations is iTZ + dT = bT
# Define the identity matrix of appropriate dimension
i <- diag(3)
 
# Solve for vector d_b
b <- matrix(c(1097, 453, 117), ncol = 3)

d_b <- solve(t(i) %*% Z,t( b))

# Print the results
cat("a) Vector d for Zi + d = b:\n")
print(d_a)
cat("\nb) Vector d for iTZ + dT = bT:\n")
print(t(d_b))  # Transpose the result to match the original dimension of 'b'

#Answers
#a) Vector d for Zi + d = b:
 #[,1]
#[1,]   8.898098
#[2,] -10.772187
#[3,] -19.010206

#b) Vector d for iTZ + dT = bT:
 #[,1]      [,2]      [,3]
#[1,] 8.898098 -10.77219 -19.01021

#Question 2

# Define the matrix Z
Z <- matrix(c(350, 0, 0, 50, 250, 175, 200, 150, 550), nrow = 3, ncol = 3)

# Define the vector x
x <- c(1400,1000, 1100)

# Create the diagonal matrix x
X <- diag(x)

# Calculate the matrix of technical coefficients A
A <- X %*% solve(X - Z)

# Print the matrix A
cat("Matrix A:\n")
print(A)

#answer
#atrix A:
#[,1]       [,2]      [,3]
#[1,] 1.333333 -0.5525526 0.4084084
#[2,] 0.000000  0.0000000 0.0000000
#[3,] 0.000000 -1.2612613 1.8018018

#Question3
library(readxl)

cpi_data <- read_excel("/home/addis/Desktop/Projects/R/1810000601.xlsx")
print(cpi_data)
colnames(cpi_data)

 
# Clean and reshape the 
print(cpi_data$Geography)
# Filter the data to select the row with Geography "All-items 8"
 

# Print the selected row
print(cpi_ts)
print(cpixfe_ts)
# Filter the data for the selected series (CPI and CPIXFE)

selected_data
# A Create time series for CPI and CPIXFE
cpi_ts <- ts(cpi_data[cpi_data$Geography == "All-items 8", -1], start = c(1993, 1), frequency = 12)
cpixfe_ts <- ts(cpi_data[cpi_data$Geography == "All-items excluding food and energy", -1], start = c(1993, 1), frequency = 12)


# Convert the time series to a vector
cpi_vector <- as.vector(cpi_ts)
cpixfe_vector<-as.vector(cpixfe_ts)


# B Calculate annual inflation rates for CPI
annual_inflation_cpi <- 100 * diff(cpi_vector, lag = 12) / cpi_vector[13:length(cpi_vector)]
annual_inflation_cpixfe<- 100 * diff(cpixfe_vector, lag = 12) / cpixfe_vector[13:length(cpixfe_vector)]
# Print the annual inflation rates
print(annual_inflation_cpi)
print(annual_inflation_cpixfe)

# Create a time sequence for the years
years <- seq.Date(from = as.Date("1993-01-01"), by = "years", length.out = length(annual_inflation_cpi))

# Create data frames for CPI and CPIXFE inflation rates with corresponding years
cpi_data <- data.frame(Year = years, Inflation = annual_inflation_cpi)
cpixfe_data <- data.frame(Year = years, Inflation = annual_inflation_cpixfe)
# Print the data frames
print(cpi_data)
print(cpixfe_data)
 
# Plot CPI inflation against years with x-axis labels
plot(cpi_data$Inflation, type = "l", main = "CPI Annual Inflation", xlab = "Year", ylab = "Inflation (%)", xaxt = "n")
# Plot CPI inflation against years with x-axis labels
plot(cpi_data$Year, cpi_data$Inflation, type = "l", main = "CPI Annual Inflation", xlab = "Year", ylab = "Inflation (%)", xaxt = "n")

# Customize x-axis labels
axis(1, at = seq(min(cpi_data$Year), max(cpi_data$Year), by = "years"), labels = format(seq(min(cpi_data$Year), max(cpi_data$Year), by = "years"), "%Y"))

# Add gridlines for better readability
grid()

# Add a line plot for CPIXFE inflation
lines(cpixfe_data$Year, cpixfe_data$Inflation, col = "red")

# Add a legend to differentiate between CPI and CPIXFE
legend("topright", legend = c("CPI", "CPIXFE"), col = c("black", "red"), lty = 1)

# Customize x-axis labels
axis(1, at = seq(min(cpi_data$Year), max(cpi_data$Year), by = "years"), labels = format(seq(min(cpi_data$Year), max(cpi_data$Year), by = "years"), "%Y"))

# Add gridlines for better readability
grid()

# Plot CPI inflation
plot(cpi_data, type = "l", main = "CPI Annual Inflation", xlab = "Year", ylab = "Inflation (%)")

# Plot CPIXFE inflation
plot(annual_inflation_cpixfe, type = "l", main = "CPIXFE Annual Inflation", xlab = "Year", ylab = "Inflation (%)")

print(cpi_data$Year)
# Reset the plotting layout
par(mfrow = c(1, 1))



# Create histograms for CPI and CPIXFE annual inflation rates
hist(annual_inflation_cpi, main = "CPI Annual Inflation Distribution", xlab = "Inflation Rate (%)", xlim = c(-5, 5), breaks = 30, col = "blue")
hist(annual_inflation_cpixfe, main = "CPIXFE Annual Inflation Distribution", xlab = "Inflation Rate (%)", xlim = c(-5, 5), breaks = 30, col = "red")

# Add a legend
legend("topright", legend = c("CPI", "CPIXFE"), fill = c("blue", "red"))

# Calculate the percentage of time inflation was within the 1-3% range
within_target_cpi <- sum(annual_inflation_cpi >= 1 & annual_inflation_cpi <= 3) / length(annual_inflation_cpi) * 100
within_target_cpixfe <- sum(annual_inflation_cpixfe >= 1 & annual_inflation_cpixfe <= 3) / length(annual_inflation_cpixfe) * 100

# Print the percentage of time within target range
cat("Percentage of time CPI inflation was within 1-3% range:", within_target_cpi, "%\n")
cat("Percentage of time CPIXFE inflation was within 1-3% range:", within_target_cpixfe, "%\n")






#C) Calculate Variance of Inflation Rates and Determine the Most Volatile Measure:

# Calculate variance of CPI and CPIXFE inflation rates
variance_cpi <- var(annual_inflation_cpi)
variance_cpixfe <- var(annual_inflation_cpixfe)

cat("Variance of CPI Inflation:", variance_cpi, "\n")
cat("Variance of CPIXFE Inflation:", variance_cpixfe, "\n")

# Determine which measure is more volatile
if (variance_cpi > variance_cpixfe) {
  cat("CPI inflation is more volatile.\n")
} else {
  cat("CPIXFE inflation is more volatile.\n")
}
#Answers
#Variance of CPI Inflation: 1.579791 
 #Variance of CPIXFE Inflation: 0.8022275 

"""
D) Explanation of the Importance of Core Inflation:
Core inflation measures, like CPIXFE, are essential for monetary policy because they exclude volatile components such as food and energy prices, which can have short-term fluctuations due to factors unrelated to underlying economic trends. Policymakers pay attention to core inflation to gain insights into the persistent and fundamental inflationary pressures in the economy. By focusing on core inflation, they can make more informed decisions about interest rates and monetary policy, as it provides a clearer signal of long-term inflation trends, allowing for a more stable and predictable economic environment.
"""
#E) Create CPITAR Index:

# Define the annual inflation target (2%)
annual_inflation_target <- 0.02

# Calculate the CPITAR index
cpitar_index <- numeric(length(cpi_ts))
cpitar_index[1] <- 100  # Set the initial value

for (i in 2:length(cpitar_index)) {
  cpitar_index[i] <- cpitar_index[i - 1] * (1 + annual_inflation_target / 12)
}
print(cpitar_index)

#G) Plot CPI, CPIXFE, and CPITAR:
  
# Renormalize CPI and CPIXFE to January 1995 = 100
cpi_renormalized <- (cpi_ts / cpi_ts["1995-01"]) * 100
cpixfe_renormalized <- (cpixfe_ts / cpixfe_ts["1995-01"]) * 100

# Create a time series for CPITAR
cpitar_ts <- ts(cpitar_index, start = c(1993, 1), frequency = 12)

 
     
# Load the ggplot2 package if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Create data frames for each series
cpi_df <- data.frame(Year = time(cpi_renormalized), CPI = cpi_renormalized)
cpixfe_df <- data.frame(Year = time(cpixfe_renormalized), CPIXFE = cpixfe_renormalized)
cpitar_df <- data.frame(Year = time(cpitar_ts), CPITAR = cpitar_ts)

 


















# Renormalize CPI and CPIXFE to January 1995 = 100
cpi_renormalized <- (cpi_ts / cpi_ts["1995-01"]) * 100
cpixfe_renormalized <- (cpixfe_ts / cpixfe_ts["1995-01"]) * 100

# Create a time series for CPITAR
cpitar_ts <- ts(cpitar_index, start = c(1992, 2), frequency = 12)  # Start in February 1992

# Plot CPI, CPIXFE, and CPITAR
plot(cpitar_ts, type = "l", col = "green", lwd = 2, ylim = c(80, 120),
     main = "CPI, CPIXFE, and CPITAR", xlab = "Year", ylab = "Price Index (Jan 1995 = 100)")

lines(cpi_renormalized, col = "blue", lwd = 2)
lines(cpixfe_renormalized, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("CPITAR", "CPI", "CPIXFE"), col = c("green", "blue", "red"), lwd = 2)

# Calculate the difference in price levels between CPI and CPITAR
price_difference_cpi <- cpi_renormalized - cpitar_ts
price_difference_cpixfe <- cpixfe_renormalized - cpitar_ts

# Print whether current price levels are higher or lower than CPITAR
cat("CPI price level vs. CPITAR in July 2023:", ifelse(price_difference_cpi[length(price_difference_cpi)] > 0, "Higher", "Lower"), "\n")
cat("CPIXFE price level vs. CPITAR in July 2023:", ifelse(price_difference_cpixfe[length(price_difference_cpixfe)] > 0, "Higher", "Lower"), "\n")










# Normalize CPI and CPIXFE to January 1995
cpi_normalized <- (cpi_ts / cpi_ts[37]) * 100
cpixfe_normalized <- (cpixfe_ts / cpixfe_ts[37]) * 100

# Plot CPI, CPIXFE, and CPITAR
ts.plot(cpi_normalized, cpixfe_normalized, cpitar_index, 
        main = "CPI, CPIXFE, and CPITAR", col = c(1, 2, 3), lty = c(1, 1, 2))
legend("topright", legend = c("CPI", "CPIXFE", "CPITAR"), col = c(1, 2, 3), lty = c(1, 1, 2))



#Question 4 

# Load necessary libraries
library(stats)
library(ggplot2)
data_t<- read_excel("/home/addis/Desktop/Projects/R/3610010301.xlsx")
data_t
colnames(data_t)
 # Convert the 'data' data frame to a time series
 
data_ts <- ts(data_t[data_t$Estimates=='Compensation of employees', -1],start = c(2012, 1), frequency = 4)
 data_ts

  data_window <- window(data_ts, start = c(2012, 1), end = c(2023, 2))
  # Plot Compensation of Employees
  # Plot the first 10 quarters (adjust the number as needed)
  plot(data_window[, 1:10], main = "Compensation of Employees (Q1 1961 - Q4 1962)")





  # Calculate the "profits" series
  
  data_t$Profits <- data_t$Estimates == "Gross operating surplus" | data_t$Estimates == "Gross mixed income"
  
  # Convert the 'Profits' variable to a time series
  profits_ts <- ts(data_t$Profits, start = c(2012, 1), frequency = 4)
  
  # Select the same time window as in part A
  profits_window <- window(profits_ts, start = c(2012, 1), end = c(2023, 2))
  
  # Plot the "profits" series
  plot(profits_window, main = "Profits (Q1 2012 - Q2 2023)")
  
  
  
  # Calculate the "GDP_market_prices" variable
  data_tc<-(data_t[data_t$Estimates == 'Compensation of employees',-1]) 
  data_tc
  
  # Calculate the "GDP_market_prices" variable
  data_t$GDP_market_prices <- data_t$Estimates %in% c(
    'Compensation of employees',
    'Gross operating surplus',
    'Gross mixed income',
    'Taxes less subsidies on production 1',
    'Taxes less subsidies on products and imports 1',
    'Statistical discrepancy'
  )
  
  data_t$GDP_market_prices
  
  # Calculate the labor income share and profit share of income
  data_t$Labor_share <- ifelse(data_t$GDP_market_prices != 0,
                               (data_t$Estimates == 'Compensation of employees') / data_t$GDP_market_prices * 100,
                               NA)
  data_t$Profit_share <- ifelse(data_t$GDP_market_prices != 0,
                                ((data_t$Estimates == "Gross operating surplus") | (data_t$Estimates == "Gross mixed income")) / data_t$GDP_market_prices * 100,
                                NA)
  data_t$Labor_share
  
  # Convert the new variables to time series
  labor_share_ts <- ts(data_t$Labor_share, start = c(2012, 1), frequency = 4)
  profit_share_ts <- ts(data_t$Profit_share, start = c(2012, 1), frequency = 4)
  
  # Select the same time window as in part A
  labor_share_window <- window(labor_share_ts, start = c(2012, 1), end = c(2023, 2))
  profit_share_window <- window(profit_share_ts, start = c(2012, 1), end = c(2023, 2))
  
  # Plot the labor income share and profit share of income on the same graph
  plot(labor_share_window, col = "blue", type = "l", lwd = 2, ylim = c(40, 70), ylab = "Percentage")
  lines(profit_share_window, col = "red", type = "l", lwd = 2)
  legend("topright", legend = c("Labor Share", "Profit Share"), col = c("blue", "red"), lwd = 2, bty = "n")
  title("Labor Share vs. Profit Share (Q1 2012 - Q2 2023)")
  
  # Select the entire time series for labor and profit share
  labor_share_full <- ts(data_t$Labor_share, start = c(1961, 1), frequency = 4)
  profit_share_full <- ts(data_t$Profit_share, start = c(1961, 1), frequency = 4)
  
  # Plot the labor income share and profit share of income for the entire sample
  plot(labor_share_full, col = "blue", type = "l", lwd = 2, ylim = c(40, 70), ylab = "Percentage")
  lines(profit_share_full, col = "red", type = "l", lwd = 2)
  legend("topright", legend = c("Labor Share", "Profit Share"), col = c("blue", "red"), lwd = 2, bty = "n")
  title("Labor Share vs. Profit Share (1961Q1 - 2023Q2)")
  
  # Show the Labor_share values
  data_t$Labor_share
  





#C) By plotting the labor income share and profit share of income from Q1 2012 to Q2 2023, you can observe that there has been an increase in the profit share of income since 2021. This indicate a potential shift in income distribution.

#D) By extending the analysis to the entire sample from 1961Q1 to 2023Q2, we compare the historical trends in the labor and profit share of income with the more recent trends observed in part C. This shows that the recent changes are consistent with long-term pattern.



#Question 5

#A) Construct the matrix D:
# Define the exogenous parameters
  s <- 0.20
  alpha <- 0.4
  delta <- 0.1
  A <- 2
  

# Create the matrix D
D <- matrix(0, nrow = 6, ncol = 6)

# Fill in the matrix D with coefficients  
D[1, 2] <- 1
D[3,3]<-0
D[2,1]<-1
D[1,3]<--1
D[3,1]<--(1-s)
D[2, 1] <- 1
D[4,1]<-1
D[6,1]<--(1-a)
D[4,3]<--1
D[4,5]<--0
D[5,5]<-1
D[6,6]<-1
D[4, 4] <- -1
D[5, 5] <- 1
D[5, 6] <- 0
D[6, 1] <- -(1 - delta)
D[6, 3] <- 0
D[6, 6] <- 1

# Print the matrix D
D<-matrix(c(D),nrow = 6,ncol = 6,byrow = TRUE)

#B) Construct the vector b assuming k₀ = 1:

# Assume an initial value for capital stock (k0)
k0 <- 1

# Calculate the other elements of vector b
b <- matrix(c((1 - delta) * k0, A * k0^alpha, 0, 0, alpha * A * k0^(alpha - 1),0),byrow = TRUE)
# Print the vector b
print(b)



#C) Find the value of endogenous variables for period t = 1:

# Calculate the endogenous variables for period t = 1
x <- solve(D, b)

# Print the values of endogenous variables for t = 1
print(x)


#D) Calculate the vector b for period t = 2:

# Update the vector b for t = 2
k1 <- x[2]  # Capital stock for t = 1 is in x[2]

# Calculate the other elements of vector b for t = 2
b <- c((1 - delta) * k1, A * k1^alpha, 0, 0, alpha * A * k1^(alpha - 1),0)

# Print the updated vector b for t = 2
print(b)


""
E) Use a loop to calculate endogenous variables for periods t = 1 to 200 and identify the steady state
"""
  
# Initialize variables
k <- k0  # Initial capital stock
tolerance <- 1e-6  # Tolerance for convergence
max_iterations <- 200

# Create vectors to store values over time
k_values <- numeric(max_iterations)
y_values <- numeric(max_iterations)
c_values <- numeric(max_iterations)
R_values <- numeric(max_iterations)
w_values <- numeric(max_iterations)

for (t in 1:max_iterations) {
  # Update vector b for the current period
  b <- c((1 - delta) * k, A * k^alpha, 0, 0, alpha * A * k^(alpha - 1),0)
  
  # Calculate endogenous variables for the current period
  x <- solve(D, b)
  
  # Store values for the current period
  k_values[t] <- k
  y_values[t] <- x[1]
  c_values[t] <- x[4]
  R_values[t] <- alpha * A * k^(alpha - 1)
  w_values[t] <- (1 - alpha) * x[1]
  
  # Check for convergence to steady state
  if (t > 1 && abs(k - k_values[t - 1]) < tolerance) {
    break  # Steady state reached
  }
  
  # Update capital for the next period
  k <- x[2]
}

 
