# BDA400 Assignment 5 - Moving Average Convergence Divergence (MACD)
# Requires ema() from ema.R. Uses only base R.

macd <- function(data, short_period, long_period, signal_period) {
  if (!exists("ema", mode = "function")) stop("Source ema.R before using macd()")
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")

  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}
