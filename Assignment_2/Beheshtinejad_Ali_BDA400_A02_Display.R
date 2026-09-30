# ============================================================
# BDA400 - Assignment 2
# Data Display and Visualization
# Student: Ali Beheshtinejad
# ============================================================

# Run Beheshtinejad_Ali_BDA400_A02.R first.
if (!exists("stocks") || !exists("all_statistics")) {
  stop("Run Beheshtinejad_Ali_BDA400_A02.R first.")
}

for (symbol in names(stocks)) {
  display_stock_data(symbol, stocks[[symbol]])
  display_statistics(symbol, all_statistics[[symbol]])
  plot_stock_data(symbol, stocks[[symbol]], ma_period = 20)
}

# Additional example of a different stock-data display:
if ("AAPL" %in% names(stocks)) {
  plot_volume("AAPL", stocks[["AAPL"]])
}
