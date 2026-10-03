# BDA400 Assignment 5 - Stochastic RSI (StochRSI)
# Requires rsi() from rsi.R and sma() from sma.R. Uses only base R.

stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function")) stop("Source rsi.R before using stoch_rsi()")
  if (!exists("sma", mode = "function")) stop("Source sma.R before using stoch_rsi()")

  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Work with valid RSI values because the initial RSI positions are NA
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) < k_period) stop("Not enough valid RSI values for k_period")

  # Normalize RSI values between 0 and 1
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)

  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line
  k_line <- sma(k_values, k_period)

  if (length(k_line) < d_period) stop("Not enough %K values for d_period")

  # Calculate the %D line
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
