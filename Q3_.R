# Replace these values with your calculated values
d <- 0.33
f <- 0.035

# Transition matrix
transition_matrix <- matrix(c(1 - d, d, f, 1 - f), nrow = 2, byrow = TRUE)
# Number of years to iterate
num_years <- 10

# Initial state
initial_state <- c(1, 0)

# Create a matrix to store results
result_matrix <- matrix(0, nrow = num_years, ncol = 2)

# Iterate the Markov process
for (i in 1:num_years) {
  # Update the state
  current_state <- initial_state %*% transition_matrix
  
  # Store the result in the matrix
  result_matrix[i, ] <- current_state
  
  # Update the initial state for the next iteration
  initial_state <- current_state
}

# Print the result
print(result_matrix)

