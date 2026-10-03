# BDA400 Assignment 5 - Standard Deviation (stdev)
# Implements the population standard deviation formula from the assignment.

stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")

  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
