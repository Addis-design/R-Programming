library(sfcr)

# Remove all existing objects in the workspace
rm(list=ls())

#Question 1
reg_eqs <- sfcr_set(
  Y_AB ~ C_AB + G_AB + X_AB - M_AB,
  Y_ON ~ C_ON + G_ON + X_ON - M_ON,
  Y_QC ~ C_QC + G_QC + X_QC - M_QC,
  Y ~ Y_AB + Y_ON + Y_QC,
  M_AB_ON ~ mu_AB_ON * Y_AB,
  M_AB_QC ~ mu_AB_QC * Y_AB,
  M_AB ~ M_AB_ON + M_AB_QC,
  M_ON_AB ~ mu_ON_AB * Y_ON,
  M_ON_QC ~ mu_ON_QC * Y_ON,
  M_ON ~ M_ON_AB + M_ON_QC,
  M_QC_AB ~ mu_QC_AB * Y_QC,
  M_QC_ON ~ mu_QC_ON * Y_QC,
  M_QC ~ M_QC_ON + M_QC_AB,
  X_AB_ON ~ M_ON_AB,
  X_AB_QC ~ M_QC_AB,
  X_AB ~ X_AB_ON + X_AB_QC,
  X_ON_AB ~ M_AB_ON,
  X_ON_QC ~ M_QC_ON,
  X_ON ~ X_ON_AB + X_ON_QC,
  X_QC_AB ~ M_AB_QC,
  X_QC_ON ~ M_ON_QC,
  X_QC ~ X_QC_AB + X_QC_ON,
  YD_AB ~ Y_AB - TX_AB + r[-1] * Bh_AB[-1] + EQZ_AB,
  YD_ON ~ Y_ON - TX_ON + r[-1] * Bh_ON[-1] + EQZ_ON,
  YD_QC ~ Y_QC - TX_QC + r[-1] * Bh_QC[-1] + EQZ_QC,
  TX_AB ~ theta * ( Y_AB + r[-1] * Bh_AB[-1] ),
  TX_ON ~ theta * ( Y_ON + r[-1] * Bh_ON[-1] ),
  TX_QC ~ theta * ( Y_QC + r[-1] * Bh_QC[-1] ),
  V_AB ~ V_AB[-1] + ( YD_AB - C_AB ),
  V_ON ~ V_ON[-1] + ( YD_ON - C_ON ),
  V_QC ~ V_QC[-1] + ( YD_QC - C_QC ),
  C_AB ~ alpha1_AB * YD_AB + alpha2_AB * V_AB[-1],
  C_ON ~ alpha1_ON * YD_ON + alpha2_ON * V_ON[-1],
  C_QC ~ alpha1_QC * YD_QC + alpha2_QC * V_QC[-1],
  Hh_AB ~ V_AB - Bh_AB,
  Hh_ON ~ V_ON - Bh_ON,
  Hh_QC ~ V_QC - Bh_QC,
  Bh_AB ~ V_AB * ( lambda0_AB + lambda1_AB * r ),
  Bh_ON ~ V_ON * ( lambda0_ON + lambda1_ON * r ),
  Bh_QC ~ V_QC * ( lambda0_QC + lambda1_QC * r ),
  TX ~ TX_AB + TX_ON + TX_QC,
  G ~ G_AB + G_ON + G_QC,
  Bh ~ Bh_AB + Bh_ON + Bh_QC,
  Hh ~ Hh_AB + Hh_ON + Hh_QC,
  Bs ~ Bs[-1] + ( G + EQZ_CA + r[-1] * Bs[-1] ) - ( TX + r[-1] * Bcb[-1] ),
  Hs ~ Hs[-1] + Bcb - Bcb[-1],
  Bcb ~ Bs - Bh,
  AVGY ~ Y/3, # Moyenne Canadienne / Canadian Average
  zeta_AB ~ if ( Y_AB[-1]-AVGY[-1] <0 ) {1} else {0},
  zeta_ON ~ if ( Y_ON[-1]-AVGY[-1] <0 ) {1} else {0},
  zeta_QC ~ if ( Y_QC[-1]-AVGY[-1] <0 ) {1} else {0},
  EQZ_AB ~ -zeta_AB*rho_eqz*(Y_AB[-1]-AVGY[-1]),
  EQZ_ON ~ -zeta_ON*rho_eqz*(Y_ON[-1]-AVGY[-1]),
  EQZ_QC ~ -zeta_QC*rho_eqz*(Y_QC[-1]-AVGY[-1]),
  EQZ_CA ~ EQZ_AB + EQZ_ON + EQZ_QC,
  redondant ~ Hs - Hh
)
reg_ext <- sfcr_set(
  r ~ 0.045,
  G_AB ~ 50,
  G_ON ~ 50,
  G_QC ~ 50,
  rho_eqz ~ 0,
  mu_AB_ON ~ 0.2,
  mu_AB_QC ~ 0.2,
  mu_ON_AB ~ 0.2,
  mu_ON_QC ~ 0.2,
  mu_QC_AB ~ 0.2,
  mu_QC_ON ~ 0.2,
  alpha1_AB ~ 0.80,
  alpha1_ON ~ 0.60,
  alpha1_QC ~ 0.70,
  alpha2_AB ~ 0.20,
  alpha2_ON ~ 0.40,
  alpha2_QC ~ 0.30,
  lambda0_AB ~ 0.7,
  lambda0_ON ~ 0.7,
  lambda0_QC ~ 0.7,
  lambda1_AB ~ 0.08,
  lambda1_ON ~ 0.08,
  lambda1_QC ~ 0.08,
  theta ~ 0.30
)
# Combine the equations and parameters into a single model using sfcr_baseline
model <- sfcr_baseline(equations = reg_eqs, external = reg_ext, periods = 100, method = "Broyden")

# Print the model summary to check if it's correctly defined
print(model)
# a) Simulate the model for 100 periods and find the steady state values of GDP in each province
sim_result_a <- sfcr_baseline(equations = reg_eqs, external = reg_ext, periods = 100, method = "Broyden")

# Steady state values of GDP in each province
steady_state_gdp_AB <- mean(sim_result_a$Y_AB[91:100])
steady_state_gdp_ON <- mean(sim_result_a$Y_ON[91:100])
steady_state_gdp_QC <- mean(sim_result_a$Y_QC[91:100])

# Display the steady state values
cat("Steady State GDP in Alberta:", steady_state_gdp_AB, "\n")
cat("Steady State GDP in Ontario:", steady_state_gdp_ON, "\n")
cat("Steady State GDP in Quebec:", steady_state_gdp_QC, "\n")

# b) Canada’s consumption at steady state
steady_state_consumption_CAN <- mean(sim_result_a$C_AB[91:100])
cat("Canada’s Consumption at Steady State:", steady_state_consumption_CAN, "\n")

# c) Simulate the impact of increasing spending in Alberta (G_AB) by 10%
# Combine the equations and parameters into a single model using sfcr_baseline
model <- sfcr_baseline(equations = reg_eqs, external = reg_ext, periods = 100, method = "Broyden")

# Simulate the baseline model
baseline_result <- sfcr_baseline(equations = reg_eqs, external = reg_ext, periods = 100, method = "Broyden")

# Create a shock scenario: Increase spending in Alberta (G_AB) by 10%
shock_values <- baseline_result$G_AB * 0.1

# Initialize the shock result as a copy of the baseline result
# Calculate the steady state GDP for Canada
steady_state_gdp_CAN <- mean(baseline_result$Y[91:100])

# Create a shock scenario: Increase spending in Alberta (G_AB) by 10%
shock_values <- baseline_result$G_AB * 0.1

# Initialize the shock result as a copy of the baseline result
shock_result_c <- baseline_result

# Apply the shock manually using a loop
for (t in 3:50) {
  # Create a copy of the baseline result to avoid modification issues
  shock_result_copy <- shock_result_c
  
  # Modify the shock copy
  shock_result_copy$G_AB[t] <- baseline_result$G_AB[t] + shock_values[t]
  
  # Simulate the model with the shock
  shock_result_copy <- sfcr_baseline(equations = reg_eqs, external = reg_ext, initial = shock_result_copy, periods = 100, method = "Broyden")
  
  # Update the shock result
  shock_result_c <- shock_result_copy
}

# Plot Canadian GDP following the fiscal policy in % deviation from the initial steady state
plot(1:10, (shock_result_c$Y[1:10] - steady_state_gdp_CAN) / steady_state_gdp_CAN * 100,
     type = "l", col = "blue", xlab = "Periods", ylab = "GDP Deviation (%)",
     main = "Impact of 10% Increase in G_AB on Canadian GDP")



# d) Simulate the impact of increasing spending in Ontario (G_ON) by 10%
# Calculate the steady state GDP for Canada
steady_state_gdp_CAN <- mean(baseline_result$Y[91:100])

# Create a shock scenario: Increase spending in Ontario (G_ON) by 10%
shock_values_on <- baseline_result$G_ON * 0.1

# Initialize the shock result as a copy of the baseline result
shock_result_on <- baseline_result

# Apply the shock manually using a loop
for (t in 3:50) {
  # Create a copy of the baseline result to avoid modification issues
  shock_result_copy_on <- shock_result_on
  
  # Modify the shock copy
  shock_result_copy_on$G_ON[t] <- baseline_result$G_ON[t] + shock_values_on[t]
  
  # Simulate the model with the shock
  shock_result_copy_on <- sfcr_baseline(equations = reg_eqs, external = reg_ext, initial = shock_result_copy_on, periods = 100, method = "Broyden")
  
  # Update the shock result
  shock_result_on <- shock_result_copy_on
}

# Plot Canadian GDP following the fiscal policy in % deviation from the initial steady state
plot(1:10, (shock_result_on$Y[1:10] - steady_state_gdp_CAN) / steady_state_gdp_CAN * 100,
     type = "l", col = "green", xlab = "Periods", ylab = "GDP Deviation (%)",
     main = "Impact of 10% Increase in G_ON on Canadian GDP")

# e) Compare the impact of fiscal policies in c) and d) for the first 6 observations
impact_c_first_6 <- shock_result_c$Y[1:6] - steady_state_gdp_CAN
impact_d_first_6 <- shock_result_on$Y[1:6] - steady_state_gdp_CAN

# Display the results
cat("Impact on Canadian GDP in c) for observations 1-6:", impact_c_first_6, "\n")
cat("Impact on Canadian GDP in d) for observations 1-6:", impact_d_first_6, "\n")

#f Plot GDP of each province and Canadian average following the fiscal spending shock in Ontario
plot(1:10, shock_result_on$Y_AB[1:10], type = "l", col = "red", xlab = "Periods", ylab = "GDP",
     main = "Impact of 10% Increase in G_ON on Provincial GDP")
lines(1:10, shock_result_on$Y_ON[1:10], col = "green")
lines(1:10, shock_result_on$Y_QC[1:10], col = "blue")
lines(1:10, shock_result_on$AVGY[1:10], col = "black", lty = 2, lwd = 2, add = TRUE)
legend("topright", legend = c("Alberta", "Ontario", "Quebec", "Canadian Average"),
       col = c("red", "green", "blue", "black"), lty = c(1, 1, 1, 2), lwd = c(1, 1, 1, 2))

# g) The parameter that determines whether or not the government provides equalization payments is zeta_j, where j represents a province. This parameter is involved in the calculation of EQZ_j in the model.
#zeta_j is the parameter that determines whether or not the government ever makes equalization payments to a given province j. This parameter is used in the calculation of EQZ_j in the model.

# h) Answer question d) with the parameter zeta_j taking on the value of 1.
# Set zeta_ON to 1
reg_ext$zeta_ON <- 1
# Simulate the model with the shock manually using a loop
shock_result_d_with_eqz <- baseline_result

# Apply the shock manually using a loop
for (t in 3:50) {
  # Create a copy of the baseline result to avoid modification issues
  shock_result_copy <- shock_result_d_with_eqz
  
  # Modify the shock copy
  shock_result_copy$G_ON[t] <- baseline_result$G_ON[t] + shock_values_on[t]
  
  # Set zeta_ON to 1
  shock_result_copy$zeta_ON[t] <- 1
  
  # Simulate the model with the shock
  shock_result_copy <- sfcr_baseline(equations = reg_eqs, external = reg_ext, initial = shock_result_copy, periods = 100, method = "Broyden")
  
  # Update the shock result
  shock_result_d_with_eqz <- shock_result_copy
}

# Extract the first 10 observations for plotting
shock_result_d_with_eqz_first_10 <- shock_result_d_with_eqz[, 1:10]

# i) Plot the evolution of Ontario GDP as found in d) and h)
plot(1:10, (shock_result_on$Y[1:10] - steady_state_gdp_CAN) / steady_state_gdp_CAN * 100,
     type = "l", col = "green", xlab = "Periods", ylab = "GDP Deviation (%)",
     main = "Impact of 10% Increase in G_ON on Ontario GDP")
lines(1:10, (shock_result_d_with_eqz_first_10$Y - steady_state_gdp_CAN) / steady_state_gdp_CAN * 100, col = "blue")
legend("topright", legend = c("Without Equalization Payments", "With Equalization Payments"),
       col = c("green", "blue"), lty = 1)

