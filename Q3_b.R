# Define the transition matrix
transition_matrix <- matrix(c(0.67, 0.33, 0.035, 0.965), nrow = 2, byrow = TRUE)

# Initial state [1 0]
initial_state <- c(1, 0)

# Calculate the probability of transitioning to Success
probability_success <- initial_state %*% transition_matrix[, 1]
probability_success
# Calculate the probability of transitioning from [1 0] to [1 0] (Success in both years)
probability_two_successive_years <- initial_state %*% transition_matrix %*% transition_matrix[, 1]
probability_two_successive_years
