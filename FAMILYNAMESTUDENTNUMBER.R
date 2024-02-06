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



#Question 2


open_eqs <- sfcr_set(
  eq1 = Y_CA ~ C_CA + G_CA + X_CA - M_CA,
  eq2 = Y_US ~ C_US + G_US + X_US - M_US,
  eq3 = M_CA ~ mu_CA * Y_CA,
  eq4 = M_US ~ mu_US * Y_US,
  eq5 = X_CA ~ M_US / xr,
  eq6 = X_US ~ M_CA * xr,
  eq7 = YD_CA ~ Y_CA + r_CA[-1] * Bh_CA_CA[-1] + r_US[-1] * Bs_CA_US[-1] / xr - TX_CA,
  eq8 = YD_US ~ Y_US + r_CA[-1] * Bs_US_CA[-1] * xr + r_US[-1] * Bh_US_US[-1] - TX_US,
  eq9 = TX_CA ~ theta_CA * ( Y_CA + r_CA[-1] * Bh_CA_CA[-1] + r_US[-1] * Bs_CA_US[-1] / xr ),
  eq10 = TX_US ~ theta_US * ( Y_US + r_CA[-1] * Bs_US_CA[-1] * xr + r_US[-1] * Bh_US_US[-1] ),
  eq11 = CG_CA ~ Bs_CA_US[-1] * (1/xr - 1/xr[-1]),
  eq12 = CG_US ~ Bs_US_CA[-1] * (xr - xr[-1]),
  eq13 = V_CA ~ V_CA[-1] + ( YD_CA - C_CA ) + CG_CA,
  eq14 = V_US ~ V_US[-1] + ( YD_US - C_US ) + CG_US,
  eq15 = C_CA ~ alpha1_CA * YD_CA + alpha2_CA * V_CA[-1],
  eq16 = C_US ~ alpha1_US * YD_US + alpha2_US * V_US[-1],
  eq17 = Hh_CA ~ V_CA - Bh_CA_CA - Bh_CA_US,
  eq18 = Hh_US ~ V_US - Bh_US_CA - Bh_US_US,
  eq19 = Bh_CA_CA ~ V_CA * ( lambda20_CA + lambda22_CA * r_CA + lambda23_CA * r_US ),
  eq20 = Bh_CA_US ~ V_CA * ( lambda30_CA + lambda32_CA * r_CA + lambda33_CA * r_US ),
  eq21 = Bh_US_CA ~ V_US * ( lambda20_US + lambda22_US * r_CA + lambda23_US * r_US ),
  eq22 = Bh_US_US ~ V_US * ( lambda30_US + lambda32_US * r_CA + lambda33_US * r_US ),
  eq23 = Bs_CA ~ Bs_CA[-1] + ( G_CA + r_CA[-1] * Bs_CA[-1] ) - ( TX_CA + r_CA[-1] * Bcb_CA[-1] ),
  eq24 = Bs_US ~ Bs_US[-1] + ( G_US + r_US[-1] * Bs_US[-1] ) - ( TX_US + r_US[-1] * Bcb_US[-1] ),
  eq25 = Bs_CA_US ~ Bh_CA_US * xr,
  eq26 = Bs_US_CA ~ Bh_US_CA / xr,
  eq27 = Bcb_CA ~ Bs_CA - Bh_CA_CA - Bs_US_CA,
  eq28 = Bcb_US ~ Bs_US - Bs_CA_US - Bh_US_US,
  eq29 = or_CA ~ or_CA[-1] + ( Hs_CA - Hs_CA[-1] - ( Bcb_CA - Bcb_CA[-1] ) )/pg_CA,
  eq30 = or_US ~ or_US[-1] + ( Hs_US - Hs_US[-1] - ( Bcb_US - Bcb_US[-1] ) )/pg_US,
  eq31 = Hs_CA ~ Hh_CA,
  eq32 = Hs_US ~ Hh_US,
  eq33 = xr ~ pg_US / pg_CA,
  eq34 = deltaor_US ~ or_US - or_US[-1],
  eq35 = deltaor_CA ~ or_CA - or_CA[-1],
  eq36 = CAB_CA ~ X_CA - M_CA + r_US[-1] * Bs_CA_US[-1] * xr - r_CA[-1] * Bs_US_CA[-1],
  eq37 = CAB_US ~ X_US - M_US - r_US[-1] * Bs_CA_US[-1] + r_CA[-1] * Bs_US_CA[-1] / xr,
  eq38 = FAB_CA ~ -(deltaor_CA*pg_CA + (Bs_CA_US - Bs_CA_US[-1])*xr - (Bs_US_CA - Bs_US_CA[-1])),
  eq39 = FAB_US ~ -(deltaor_US*pg_US + (Bs_US_CA - Bh_US_CA[-1])/xr - (Bs_CA_US - Bh_CA_US[-1])),
  eq40 = BOP_CA ~ CAB_CA + FAB_CA,
  eq41 = BOP_US ~ CAB_US + FAB_US,
  eq42 = GDEF_CA ~ -(Bs_CA - Bs_CA[-1]),
  eq43 = GDEF_US ~ -(Bs_US - Bs_US[-1]),
  eq44 = redondant ~ deltaor_US + deltaor_CA
)
open_ext <- sfcr_set(
  pg_CA ~ 1.0,
  pg_US ~ 1.0,
  r_CA ~ 0.04,
  r_US ~ 0.04,
  orbar_CA ~ 100,
  orbar_US ~ 100,
  G_US ~ 50,
  G_CA ~ 50,
  mu_CA ~ 0.15,
  mu_US ~ 0.15,
  alpha1_CA ~ 0.8,
  alpha1_US ~ 0.8,
  alpha2_CA ~ 0.2,
  alpha2_US ~ 0.2,
  lambda20_CA ~ 0.55,
  lambda30_CA ~ 0.25,
  lambda20_US ~ 0.25,
  lambda30_US ~ 0.55,
  lambda22_CA ~ 5,
  lambda23_CA ~ -3,
  lambda32_CA ~ -3,
  lambda33_CA ~ 5,
  lambda22_US ~ 5,
  lambda23_US ~ -3,
  lambda32_US ~ -3,
  lambda33_US ~ 5,
  theta_CA ~ 0.2,
  theta_US ~ 0.2,
)
open_initial <- sfcr_set(
  xr ~ 1,
  or_CA ~ 100,
  or_US ~ 100
)
# Simulate the model for 200 periods
model <- sfcr_baseline(
  equations = open_eqs,
  external = open_ext,
  initial = open_initial,
  periods = 200,
  method = "Broyden"
)
# a) Determine the exchange rate direction
steady_state_xr <- tail(model$xr, 1)
if (steady_state_xr > 1) {
  cat("Exchange rate quoted as the number of US dollars needed to buy 1 Canadian dollar.\n")
} else {
  cat("Exchange rate quoted as the number of Canadian dollars needed to buy 1 US dollar.\n")
}


#a) Exchange Rate Direction
#Based on the model definition, xr represents the number of US dollars needed to buy 1 Canadian dollar.
#b) Explanation of Parameters lambda20_CA, lambda30_CA, lambda20_US, and lambda30_US
#These parameters represent the coefficients associated with the impact of the real interest rate on the demand for government bills.



#c) Intuition behind Calibration of lambda20_CA and lambda30_CA
#The fact that lambda20_CA is greater than lambda30_CA indicates the direct impact of Canada's real interest rate on Canadian government bills is stronger than the indirect effect through exchange rates. If we assume that households are more sensitive to fluctuations in the domestic real interest rate, this calibration is reasonable.

#In the same vein, lambda30_US >lambda20 is indicative of a greater indirect effect via the exchange rate than on US government bills in response to higher real interest rates. This calibration is reasonable to assume that households are more sensitive than governments when it comes to changes in the exchange rate.
#d) Share of Wealth for Canadian Households at Steady State

# Steady-state values
lambda20_CA <- 0.55
lambda30_CA <- 0.25
lambda20_US <- 0.25
lambda30_US <- 0.55

V_CA_steady <- 100  # Assuming a steady-state value for V_CA
Bh_CA_CA_steady <- V_CA_steady * (lambda20_CA)
Bh_CA_US_steady <- V_CA_steady * (lambda30_CA)

Hh_CA_steady <- V_CA_steady * (1 - lambda20_CA - lambda30_CA)

# Calculate shares
share_Bh_CA_CA <- Bh_CA_CA_steady / V_CA_steady
share_Bh_CA_US <- Bh_CA_US_steady / V_CA_steady
share_Hh_CA <- Hh_CA_steady / V_CA_steady

# Print shares
cat("Share of wealth in Canadian government bills (Bh_CA_CA):", share_Bh_CA_CA, "\n")
cat("Share of wealth in US government bills (Bh_CA_US):", share_Bh_CA_US, "\n")
cat("Share of wealth in money (Hh_CA):", share_Hh_CA, "\n")

#e) Share of Wealth for US Households at Steady State
# Steady-state values
lambda20_US <- 0.25
lambda30_US <- 0.55

V_US_steady <- 100  # Assuming a steady-state value for V_US
Bh_US_CA_steady <- V_US_steady * (lambda20_US)
Bh_US_US_steady <- V_US_steady * (lambda30_US)

Hh_US_steady <- V_US_steady * (1 - lambda20_US - lambda30_US)

# Calculate shares
share_Bh_US_CA <- Bh_US_CA_steady / V_US_steady
share_Bh_US_US <- Bh_US_US_steady / V_US_steady
share_Hh_US <- Hh_US_steady / V_US_steady

# Print shares
cat("Share of wealth in Canadian government bills (Bh_US_CA):", share_Bh_US_CA, "\n")
cat("Share of wealth in US government bills (Bh_US_US):", share_Bh_US_US, "\n")
cat("Share of wealth in money (Hh_US):", share_Hh_US, "\n")


# f) Simulate the shock: US central bank increases policy interest rate from 4% to 5%
# Simulate the shock: US central bank increases policy interest rate from 4% to 5%
# Simulate the shock: US central bank increases policy interest rate from 4% to 5%
shock_model <- sfcr_simulate(model, shocks = list(r_US = list(start = 3, end = 100, value = 0.05)))

# Plot the response of Canada and US consumption to this shock
plot(1:100, (shock_model$C_CA - tail(model$C_CA, 1)) / tail(model$C_CA, 1) * 100, type = "l", col = "blue", xlab = "Periods", ylab = "Consumption Deviation (%)", main = "Response to Policy Interest Rate Shock")

# Plot Bh_CA_CA, Bh_CA_US, Bh_US_CA, Bh_US_US on the same graph
plot(1:200, shock_model$Bh_CA_CA, type = "l", col = "red", xlab = "Periods", ylab = "Bills", main = "Government Bills Response")
lines(1:200, shock_model$Bh_CA_US, col = "blue")
legend("topright", legend = c("Bh_CA_CA", "Bh_CA_US"), col = c("red", "blue"), lty = 1)

# Plot Bh_US_CA, Bh_US_US on another graph
plot(1:200, shock_model$Bh_US_CA, type = "l", col = "green", xlab = "Periods", ylab = "Bills", main = "Government Bills Response")
lines(1:200, shock_model$Bh_US_US, col = "purple")
legend("topright", legend = c("Bh_US_CA", "Bh_US_US"), col = c("green", "purple"), lty = 1)



#Question 3
library(sfcr)

rm(list=ls())

## Mod?le PC de base, pour nous donner un ?tat stationnaire 
## initial pour les variables r?elles

## Basic PC model to get initial steady state for
## all real variables

pc_eqs <- sfcr_set(
  Y ~ C + G,
  YD ~ Y - T + r[-1] * Bh[-1],
  T ~ theta * (Y + r[-1] * Bh[-1]),
  V ~  V[-1] + (YD - C), 
  C ~ alpha1 * YD + alpha2 * V[-1],
  Hh ~ V - Bh,
  Bh ~ V * (lambda0 + lambda1 * r - lambda2 * ( YD/V )),
  Bs ~ Bs[-1] + (G + r[-1] * Bs[-1]) - (T + r[-1] * Bcb[-1]),
  Hs ~ Hs[-1] + Bcb - Bcb[-1],
  Bcb ~ Bs - Bh,
  redondant ~ Hs - Hh 
)

pc_exog <- sfcr_set(
  r ~ 0.025,
  G ~ 20,
  alpha1 ~ 0.8, 
  alpha2 ~ 0.2, 
  theta ~ 0.2,  
  lambda0 ~ 0.635, 
  lambda1 ~ 5, 
  lambda2 ~ 0.01 
)

# Simulons ? partir de rien
# Simulate from scratch

nper <- 200

pcbase <- sfcr_baseline(
  equations = pc_eqs, 
  external = pc_exog, 
  periods = nper,
  method = "Broyden"
)

#######################################################
## Cr?ons le m?me mod?le avec variables r?elles
## et variables nominales. Nous voulons que le niveau
## des variables r?elles soit le m?me dans ce mod?le
## que dans le mod?le pcbase
##
## Create same model but with real and nominal var.
## We want the level of real variables be the same as 
## in model pcbase
#######################################################

pcnom_eqs <- sfcr_set(
  eq1 = Y ~ C + G, 
  eq2 = Yn ~ Y * P,
  eq3 = N ~ Y / prod,
  eq4 = W ~ xi_w*W[-1] + (1-xi_w)*(w0 + w1*(exp(100*(N/N_fe-1))-1)),
  eq5 = Wn ~ W*P,
  eq6 = WB ~ Wn * N,
  eq7 = Profits ~ Yn - WB,
  eq8 = UCn ~ WB / Y,
  eq9 = P ~ xi_p*P[-1]+(1-xi_p)*(1+markup)*UCn,
  eq10 = Pi ~ (P/P[-1]-1),
  eq11 = YDn ~ WB + Profits + r[-1] * Bh[-1] - TX,
  eq12 = YD ~ YDn / P,
  eq13 = TX ~ theta * (WB + Profits + r[-1] * Bh[-1]),
  eq14 = Vn ~ Vn[-1] + YDn - Cn,
  eq15 = V ~ Vn / P,
  eq16 = C ~ alpha1 * YD + alpha2 * V[-1],
  eq17 = Cn ~ C * P,
  eq18 = Hh ~ Vn - Bh,
  eq19 = Bh ~ Vn * (lambda0 + lambda1 * r - lambda2 * ( YDn/Vn )),
  eq20 = Bs ~ Bs[-1] + (Gn + r[-1] * Bs[-1]) - (TX + r[-1] * Bcb[-1]),
  eq21 = G ~ Gn / P,
  eq22 = Hs ~ Hs[-1] + Bcb - Bcb[-1],
  eq23 = Bcb ~ Bs - Bh,
  eq24 = r ~ rbar,
  eq25 = redondant ~ Hs - Hh 
)

nper <- 200 

pcnom_exog <- sfcr_set(
  prod ~ 1,
  N_fe ~ pcbase$Y[200],
  markup ~ 0.1,
  
  w0 ~ 1/1.1,
  w1 ~ 0.01,
  
  rbar ~ 0.025,
  Gn ~ 20,
  theta ~ 0.2,  
  
  alpha1 ~ 0.8, 
  alpha2 ~ 0.2, 
  
  lambda0 ~ 0.635, 
  lambda1 ~ 5, 
  lambda2 ~ 0.01,
  
  xi_p ~ 0.5,
  xi_w ~ 0.5
)


# On veut le m?me ?tat stationnaire pour les variable r?elles que celles obtenues dans le mod?le sans inflation
# We want the same steady state for real variables as the ones obtained in the model without inflation

pcnom_initval <- sfcr_set(
  P ~ 1,    
  Pi ~ 0,
  Y ~ pcbase$Y[nper], 
  Yn ~ pcbase$Y[nper],
  N ~ pcbase$Y[nper],
  W ~ 1/1.1,
  Wn ~ 1/1.1,
  WB ~ pcbase$Y[nper]/1.1,
  Profits ~ pcbase$Y[nper] - pcbase$Y[nper]/1.1,
  UCn ~ 1/1.1,
  YDn ~ pcbase$YD[nper],
  YD ~ pcbase$YD[nper],
  TX ~ pcbase$T[nper], # theta < 1
  Vn ~ pcbase$V[nper],
  V ~ pcbase$V[nper],
  C ~ pcbase$C[nper], # 0 < alpha2 < alpha1 < 1
  Cn ~ pcbase$C[nper],
  Hh ~ pcbase$Hh[nper],
  Bh ~ pcbase$Bh[nper],
  Bs ~ pcbase$Bs[nper],
  G ~ pcbase$G[nper],
  Hs ~ pcbase$Hs[nper],
  Bcb ~ pcbase$Bcb[nper],
  r ~ 0.025,
  redondant ~ 0 
)

pcnom <- sfcr_baseline(
  equations = pcnom_eqs, 
  external = pcnom_exog, 
  initial = pcnom_initval,
  periods = nper,
  method = "Broyden"
)

#######################################################
## M?me mod?le, avec r?gle de politique fiscale
##
## Same model, but with more complex fiscal policy
#######################################################

pcfisc_eqs <- sfcr_set(
  eq1 = Y ~ C + G, 
  eq2 = Yn ~ Y * P,
  eq3 = N ~ Y / prod,
  eq4 = W ~ xi_w*W[-1] + (1-xi_w)*(w0 + w1*(exp(100*(N/N_fe-1))-1)),
  eq5 = Wn ~ W*P,
  eq6 = WB ~ Wn * N,
  eq7 = Profits ~ Yn - WB,
  eq8 = UCn ~ WB / Y,
  eq9 = P ~ xi_p*P[-1]+(1-xi_p)*(1+markup)*UCn,
  eq10 = Pi ~ (P/P[-1]-1),
  eq11 = YDn ~ WB + Profits + r[-1] * Bh[-1] - TX,
  eq12 = YD ~ YDn / P,
  eq13 = TX ~ theta * (WB + Profits + r[-1] * Bh[-1]),
  eq14 = Vn ~ Vn[-1] + YDn - Cn,
  eq15 = V ~ Vn / P,
  eq16 = C ~ alpha1 * YD + alpha2 * V[-1],
  eq17 = Cn ~ C * P,
  eq18 = Hh ~ Vn - Bh,
  eq19 = Bh ~ Vn * (lambda0 + lambda1 * r - lambda2 * ( YDn/Vn )),
  eq20 = Bs ~ Bs[-1] + (Gn + r[-1] * Bs[-1]) - (TX + r[-1] * Bcb[-1]),
  eq21 = Gn ~ (1-fiscsw)*((1-rhog)*Gnbar + rhog*Gn[-1]) + fiscsw*(Gnbar+e_gn),
  eq22 = G ~ Gn / P,
  eq23 = Hs ~ Hs[-1] + Bcb - Bcb[-1],
  eq24 = Bcb ~ Bs - Bh,
  eq25 = r ~ rbar,
  eq26 = redondant ~ Hs - Hh,
)

nper <- 200 

pcfisc_exog <- sfcr_set(
  pcnom_exog,
  e_gn ~ 0,
  rhog ~ 0.5,
  Gnbar ~ pcnom$Gn[nper],
  fiscsw ~ 0,
  exclude = 7
)

pcfisc_initval <- sfcr_set(
  pcnom_initval,
  Gn ~ pcnom$Gn[nper]
)

pcfisc <- sfcr_baseline(
  equations = pcfisc_eqs, 
  external = pcfisc_exog, 
  initial = pcfisc_initval,
  periods = nper,
  method = "Broyden"
)


# Simulate up to here, then start answering question 3
###################################

# a) Determine the degree of price and wage rigidity
# Extract the calibrated parameters

xi_p <- pcnom$xi_p
xi_w <- pcnom$xi_w


# Check which type of rigidity is present
if (xi_p > 0 & xi_w > 0) {
  cat("The model contains both price and wage rigidities.\n")
} else if (xi_p > 0) {
  cat("The model contains price rigidity only.\n")
} else if (xi_w > 0) {
  cat("The model contains wage rigidity only.\n")
} else {
  cat("The model does not contain price or wage rigidity.\n")
}


# b) Simulate the impact of a 1% increase in productivity
# Simulate the impact of a 1% increase in productivity
# Simulate the impact of a 1% increase in productivity
prod_shock <- sfcr_shock(
  model = pcnom,
  variable = "prod",
  value = 0.01,
  start = 3,
  end = 100
)

# Plot the evolution of the price level in % deviation from initial steady state
plot(prod_shock$Pc[1:40] - 1, type = "l", col = "blue", xlab = "Periods", ylab = "% Deviation from Initial SS", 
     main = "Impact of 1% Productivity Increase on Price Level")

# c) Plot the evolution of real wage, nominal wage, and unit cost of production
plot(prod_shock$Wc[1:40] - 1, type = "l", col = "red", xlab = "Periods", ylab = "% Deviation from Initial SS", 
     main = "Impact of 1% Productivity Increase on Real Wage, Nominal Wage, and Unit Cost of Production")
lines(prod_shock$Wn[1:40] - 1, col = "green")
lines(prod_shock$UCn[1:40] - 1, col = "purple")
legend("topright", legend = c("Real Wage", "Nominal Wage", "Unit Cost"), col = c("red", "green", "purple"), lty = 1)

# d) Explanation of the impact on the price level
cat("The increase in productivity leads to a decrease in the unit cost of production, which in turn leads to a decrease in the price level.\n")

# e) Plot the evolution of real and nominal consumption
plot(prod_shock$C[1:40] - 1, type = "l", col = "blue", xlab = "Periods", ylab = "% Deviation from Initial SS", 
     main = "Impact of 1% Productivity Increase on Real and Nominal Consumption")
lines(prod_shock$Cn[1:40] - 1, col = "red")
legend("topright", legend = c("Real Consumption", "Nominal Consumption"), col = c("blue", "red"), lty = 1)

# Explanation of the impact on consumption
cat("The increase in productivity leads to higher real and nominal consumption, indicating an improvement in households' standards of living.\n")

