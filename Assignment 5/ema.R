# BDA400 Assignment 5 - Exponential Moving Average (EMA)
# Uses only base R, following the assignment template and pseudocode.

ema <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")
  if (length(period) != 1 || !is.numeric(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  period <- as.integer(period)

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Calculate EMA for the first data point
  ema_values[1] <- data[1]

  # Calculate EMA for subsequent data points
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
