# ============================================================
# BDA400 - Assignment 2
# Technical Analysis using R - Preliminary Stage
# Student: Ali Beheshtinejad
# ============================================================

library(quantmod)
library(TTR)

# ------------------------------------------------------------
# 1. Read portfolio.txt
# ------------------------------------------------------------

read_portfolio <- function(file = "Assignment_2/portfolio.txt") {
  symbols <- readLines(file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]
  symbols <- unique(toupper(symbols))
  return(symbols)
}

# ------------------------------------------------------------
# 2. Load stock data
# ------------------------------------------------------------

load_stock_data <- function(portfolio_file = "Assignment_2/portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  
  symbols <- read_portfolio(portfolio_file)
  stock_list <- list()
  
  for (symbol in symbols) {
    
    cat("Loading", symbol, "...\n")
    
    stock_xts <- getSymbols(
      symbol,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE
    )
    
    stock_df <- data.frame(
      Date = index(stock_xts),
      coredata(stock_xts),
      row.names = NULL
    )
    
    names(stock_df) <- c(
      "Date",
      "Open",
      "High",
      "Low",
      "Close",
      "Volume",
      "Adjusted"
    )
    
    stock_list[[symbol]] <- stock_df
    
    # Create a separate data frame for each stock
    assign(
      paste0(symbol, "_data"),
      stock_df,
      envir = .GlobalEnv
    )
  }
  
  return(stock_list)
}

# ------------------------------------------------------------
# 3. Mode function
# ------------------------------------------------------------

stat_mode <- function(x) {
  
  x <- x[!is.na(x)]
  
  counts <- table(x)
  
  mode_value <- as.numeric(
    names(counts)[which.max(counts)]
  )
  
  return(mode_value)
}

# ------------------------------------------------------------
# 4. Calculate required statistics
# ------------------------------------------------------------

calculate_statistics <- function(stock_df,
                                 ma_period = 20) {
  
  close_prices <- stock_df$Close
  
  moving_average <- SMA(
    close_prices,
    n = ma_period
  )
  
  statistics <- data.frame(
    
    Statistic = c(
      "20-Day Moving Average",
      "Mean",
      "Mode",
      "Median",
      "Standard Deviation"
    ),
    
    Value = c(
      tail(na.omit(moving_average), 1),
      mean(close_prices, na.rm = TRUE),
      stat_mode(close_prices),
      median(close_prices, na.rm = TRUE),
      sd(close_prices, na.rm = TRUE)
    )
  )
  
  return(
    list(
      summary = statistics,
      moving_average = moving_average
    )
  )
}

# ------------------------------------------------------------
# 5. Display stock data
# ------------------------------------------------------------

display_stock_data <- function(symbol,
                               stock_df) {
  
  cat("\n============================\n")
  cat("STOCK:", symbol, "\n")
  cat("============================\n")
  
  print(head(stock_df))
  
  cat("\nStructure:\n")
  str(stock_df)
}

# ------------------------------------------------------------
# 6. Display statistics
# ------------------------------------------------------------

display_statistics <- function(symbol,
                               statistics) {
  
  cat("\n----------------------------\n")
  cat("STATISTICS FOR:", symbol, "\n")
  cat("----------------------------\n")
  
  print(
    statistics$summary,
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 7. Plot closing price and moving average
# ------------------------------------------------------------

plot_stock_data <- function(symbol,
                            stock_df,
                            ma_period = 20) {
  
  close_prices <- stock_df$Close
  
  ma_values <- SMA(
    close_prices,
    n = ma_period
  )
  
  plot(
    stock_df$Date,
    close_prices,
    type = "l",
    xlab = "Date",
    ylab = "Closing Price",
    main = paste(
      symbol,
      "- Closing Price and",
      ma_period,
      "Day Moving Average"
    )
  )
  
  lines(
    stock_df$Date,
    ma_values,
    lwd = 2
  )
  
  legend(
    "topleft",
    legend = c(
      "Closing Price",
      paste0(ma_period, "-Day Moving Average")
    ),
    lty = c(1, 1),
    lwd = c(1, 2),
    bty = "n"
  )
}

# ------------------------------------------------------------
# 8. Run Assignment 2 analysis
# ------------------------------------------------------------

portfolio_symbols <- read_portfolio(
  "Assignment_2/portfolio.txt"
)

print(portfolio_symbols)

stocks <- load_stock_data(
  portfolio_file = "Assignment_2/portfolio.txt"
)

all_statistics <- list()

for (symbol in names(stocks)) {
  
  display_stock_data(
    symbol,
    stocks[[symbol]]
  )
  
  stats <- calculate_statistics(
    stocks[[symbol]],
    ma_period = 20
  )
  
  all_statistics[[symbol]] <- stats
  
  display_statistics(
    symbol,
    stats
  )
  
  plot_stock_data(
    symbol,
    stocks[[symbol]],
    ma_period = 20
  )
}

cat(
  "\nAssignment 2 analysis completed successfully.\n"
)