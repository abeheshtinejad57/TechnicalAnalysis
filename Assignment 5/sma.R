# BDA400 Assignment 5 - Simple Moving Average (SMA)
# Uses only base R, following the assignment template and pseudocode.

sma <- function(data, period) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (length(period) != 1 || !is.numeric(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  period <- as.integer(period)

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}
