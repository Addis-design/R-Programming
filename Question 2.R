# Define the parameters and initial values
 
theta <- 0.20  # Tax rate
alpha1 <- 0.5  # Consumption parameter
alpha2 <- 0.5  
# Initialize variables
y <- numeric(200)  # National income
v <- numeric(200)  # Wealth
pbL <- numeric(200)  # Price of long-term bonds
rbL <- numeric(200)  # Interest rate on long-term bonds

y[1] <- 0  # Initial national income
v[1] <- 0  # Initial wealth
pbL[1] <- 1  # Initial price of long-term bonds
rbL[1] <- 0.04  # Initial interest rate on long-term bonds
 
# Simulation loop for 200 periods
for (t in 2:200) {
   y[t] <- alpha1 * y[t-1] + alpha2 * v[t-1]   
  v[t] <- v[t-1] + y[t] - (theta * y[t])  #  update for wealth
  pbL[t] <- 1 / rbL[t-1]  # Example update for price of long-term bonds
  rbL[t] <- 1 / pbL[t]  # Example update for interest rate on long-term bonds
  # ... other updates based on the model equations
}

# Find steady state values
y_star <- y[200]
v_star <- v[200]
pbL_star <- pbL[200]
rbL_star <- rbL[200]

# Print steady state values
cat("Steady state values:\n")
cat("y*:", y_star, "\n")
cat("v*:", v_star, "\n")
cat("pbL*:", pbL_star, "\n")
cat("rbL*:", rbL_star, "\n")

# Plot the price of long-term bonds
plot(pbL, type = "l", main = "Price of Long-Term Bonds Over Time",
     xlab = "Time Period", ylab = "Price of Long-Term Bonds")

 start_shock <- 3
end_shock <- 50 # Interest rate on money after shock

  
# Re-plot the price of long-term bonds after the shock
plot(pbL[start_shock:(start_shock + end_shock)], type = "l",
     main = "Price of Long-Term Bonds After Shock",
     xlab = "Time Period", ylab = "Price of Long-Term Bonds")
# Simulation loop
for (t in 2:T) {
  # Update variables based on model equations
  y[t] <- ct[t] + gt[t]
  ydr[t] <- y[t] + rh[t-1] * hh[t-1] + rb[t-1] * bh[t-1] + BLh[t-1] - tt[t]
  tt[t] <- theta * (y[t] + rh[t-1] * hh[t-1] + rb[t-1] * bh[t-1] + BLh[t-1])
  vt[t] <- vt[t-1] + (ydr[t] - ct[t]) + cgt[t]
  cgt[t] <- delta * pbL[t] * BLh[t-1]
  ct[t] <- alpha1 * yder[t] + alpha2 * vt[t-1]
  ve[t] <- vt[t-1] + yder[t] - ct[t] + cge[t]
  hh[t] <- vt[t] - bh[t] - pbL[t] * BLh[t]
  hd[t] <- ve[t] - bd[t] - pbL[t] * BLd[t]
  bd[t] <- ve[t] * (lambda20 + lambda21 * rh[t-1] + lambda22 * rb[t-1] + lambda23 * RebL[t-1] + lambda24 * yder[t] / ve[t])
  bh[t] <- bd[t]
  BLd[t] <- BLs[t]
  BLh[t] <- BLd[t]
  delta_bs[t] <- (gt[t] + rb[t-1] * bs[t-1] + BLs[t-1]) - (tt[t] + (rb[t-1] - rh[t-1]) * bcb[t-1]) - delta_BLs[t] * pbL[t]
  delta_hs[t] <- delta_bcb[t]
  bcb[t] <- bs[t] - bh[t]
  BLs[t] <- BL_bar_s
  RebL[t] <- rbL[t] + chi * (pebL[t-1] - pbL[t-1]) * pbL[t-1]
  rbL[t] <- 1 / pbL[t]
  pebL[t] <- pebL[t-1] + epbl[t]
  
  # 2h) Simulate the shock of an increase in the interest rates on money (rh)
  rh_shock <- 0.02  # Adjust this value based on the shock magnitude
  if (t >= 3) {
    rh[t] <- rh_shock
  }
  
  # 2i) Plot the government deficit following the increase in rh
  deficit[t] <- government_revenues[t] - government_expenditures[t]
  
  # 2j) Simulate the reduction in gt
  gt_reduction <- 0.1  # 10% reduction in gt
  gt[t] <- gt[t] - gt_reduction * gt[t]  # Adjust government spending
  
  # Simulate the model with reduced gt
  y[t] <- ct[t] + gt[t]
  
  # 2k) Plot total government debt
  total_debt[t] <- bs[t] + pbL[t] * BLs[t]
  
  # 2l) Plot the evolution of the long term interest rate following the reduction in gt
  # Simulate the model with reduced gt
  y[t] <- ct[t] + gt[t]
  rbL[t] <- 1 / pbL[t]
  
 
 
