# Load the markovchain package
library(markovchain)

# Create a vector of rainfall data for each day of the year
rainfall <- c(0, 0, 0, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0, 0, 0)

# Define the states
states <- c("dry", "wet")

# Create a transition matrix
transmat <- matrix(c(0.7, 0.1, 0.3, 0.9), nrow = 2, dimnames = list(states, states))

# Create a markovchain object
mc <- new("markovchain", states = states, transitionMatrix = transmat)

# Fit the model to the rainfall data
mcFit <- markovchain::markovchainFit(data = rainfall, name = "mc")

# Print the estimated transition matrix
mcFit$estimate
