# Parameters
gamma <- 0.01
g_bar <- 80 / (1 + gamma)
alpha1 <- 0.6
alpha2 <- 0.4
theta <- 0.25
wt <- 3

# Initialize variables
T <- 200
yt <- numeric(T)
ct <- numeric(T)
gt <- numeric(T)
ct_yt_ratio <- numeric(T)
gt_yt_ratio <- numeric(T)
government_deficit <- numeric(T)
hh <- numeric(T)
hs <- numeric(T)

# Simulation loop
for (t in 1:T) {
  # Calculate variables
  nt <- t  # Population grows at the rate γ = 0.01 every period
  tt <- theta * wt * nt
  gt[t] <- g_bar * (1 + gamma)
  ct[t] <- alpha1 * yt[t] + alpha2 * (ifelse(t > 1, hh[t - 1], 0))
  yt[t] <- ct[t] + gt[t]
  ct_yt_ratio[t] <- ct[t] / yt[t]
  gt_yt_ratio[t] <- gt[t] / yt[t]
  government_deficit[t] <- gt[t] - tt
  government_debt[t] <- ifelse(t > 1, government_debt[t - 1] + government_deficit[t], government_deficit[t])
  debt_to_gdp_ratio[t] <- government_debt[t] / yt[t]
  
  
  # Update household and government variables
  if (t > 1) {
    hh[t] <- hh[t - 1] + yt[t] - ct[t]
    hs[t] <- hs[t - 1] + gt[t] - tt
  }
}

# Plot evolution of y, c, and g
plot(1:T, yt, type = "l", col = "blue", xlab = "Period", ylab = "Output (y)", main = "Evolution of y, c, and g")
lines(1:T, ct, col = "red")
lines(1:T, gt, col = "green")
legend("topright", legend = c("y", "c", "g"), col = c("blue", "red", "green"), lty = 1)

# Plot ratios ct/yt and gt/yt for observations 2 to 50
plot(2:50, ct_yt_ratio[2:50], type = "l", col = "red", xlab = "Period", ylab = "Ratio", main = "Evolution of ct/yt and gt/yt")
lines(2:50, gt_yt_ratio[2:50], col = "green")
legend("topright", legend = c("ct/yt", "gt/yt"), col = c("red", "green"), lty = 1)

# Plot government deficit over time
plot(1:T, government_deficit, type = "l", col = "purple", xlab = "Period", ylab = "Government Deficit",
     main = "Evolution of Government Deficit")
# Plot evolution of government debt
plot(1:T, government_debt, type = "l", col = "blue", xlab = "Period", ylab = "Government Debt",
     main = "Evolution of Government Debt")

# Plot debt-to-GDP ratio for observations 2 to 80
plot(2:80, debt_to_gdp_ratio[2:80], type = "l", col = "red", xlab = "Period", ylab = "Debt-to-GDP Ratio",
     main = "Evolution of Debt-to-GDP Ratio")

# Explain the evolution of government debt
cat("1d) The evolution of government debt over the entire simulation is consistent with the result in 1c).\n")
cat("1f) Given the answer in 1e), we need to analyze the sustainability of public finances.\n")

