# ============================================================
# BDA400 - Assignment 2
# Technical Analysis using R - Preliminary Stage
# Student: Ali Beheshtinejad
# ============================================================

# 1. Required packages
required_packages <- c("quantmod", "TTR")

install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  missing <- packages[!(packages %in% installed)]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_packages)
library(quantmod)
library(TTR)

# 2. Locate and read portfolio.txt
find_portfolio_file <- function() {
  candidates <- c("portfolio.txt", file.path("Assignment_2", "portfolio.txt"))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("portfolio.txt was not found. Run the script from the Assignment_2 folder or repository root.")
  }
  found[1]
}

read_portfolio <- function(file = find_portfolio_file()) {
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]
  symbols <- unique(toupper(symbols))

  if (length(symbols) == 0) {
    stop("portfolio.txt does not contain any stock symbols.")
  }
  symbols
}

# 3. Import stock data and store a separate data frame for each symbol
load_stock_data <- function(portfolio_file = find_portfolio_file(),
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(portfolio_file)
  stock_list <- list()

  for (symbol in symbols) {
    message("Loading ", symbol, "...")

    stock_xts <- tryCatch(
      getSymbols(symbol, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning(paste("Could not load", symbol, "-", e$message))
        NULL
      }
    )

    if (!is.null(stock_xts)) {
      stock_df <- data.frame(
        Date = index(stock_xts),
        coredata(stock_xts),
        row.names = NULL
      )

      names(stock_df) <- c(
        "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"
      )

      stock_list[[symbol]] <- stock_df

      # Required: keep each stock in a separate data frame.
      assign(paste0(symbol, "_data"), stock_df, envir = .GlobalEnv)
    }
  }

  if (length(stock_list) == 0) {
    stop("No stock data could be loaded.")
  }
  stock_list
}

# 4. Statistical mode helper
stat_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)

  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# 5. Compute required statistics
calculate_statistics <- function(stock_df, ma_period = 20) {
  if (!is.data.frame(stock_df)) {
    stop("stock_df must be a data frame.")
  }
  if (!"Close" %in% names(stock_df)) {
    stop("The data frame must contain a Close column.")
  }

  close_prices <- as.numeric(stock_df$Close)

  if (length(close_prices) < ma_period) {
    stop("Not enough observations for the selected moving-average period.")
  }

  moving_average <- TTR::SMA(close_prices, n = ma_period)

  summary_stats <- data.frame(
    Statistic = c(
      paste0(ma_period, "-Day Moving Average"),
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

  list(summary = summary_stats, moving_average = moving_average)
}

# 6. Display utilities
display_stock_data <- function(symbol, stock_df, rows = 10) {
  cat("\n====================================================\n")
  cat("STOCK:", symbol, "\n")
  cat("====================================================\n")
  cat("First", rows, "rows:\n")
  print(head(stock_df, rows))
  cat("\nLast", rows, "rows:\n")
  print(tail(stock_df, rows))
  invisible(stock_df)
}

display_statistics <- function(symbol, statistics) {
  cat("\n----------------------------------------------------\n")
  cat("STATISTICS FOR:", symbol, "\n")
  cat("----------------------------------------------------\n")
  print(statistics$summary, row.names = FALSE)
  invisible(statistics)
}

plot_stock_data <- function(symbol, stock_df, ma_period = 20) {
  close_prices <- as.numeric(stock_df$Close)
  ma_values <- TTR::SMA(close_prices, n = ma_period)

  plot(stock_df$Date, close_prices,
       type = "l",
       xlab = "Date",
       ylab = "Closing Price",
       main = paste(symbol, "- Closing Price and", ma_period, "Day Moving Average"))

  lines(stock_df$Date, ma_values, lwd = 2)

  legend("topleft",
         legend = c("Closing Price", paste0(ma_period, "-Day Moving Average")),
         lty = c(1, 1),
         lwd = c(1, 2),
         bty = "n")
}

plot_volume <- function(symbol, stock_df) {
  plot(stock_df$Date, stock_df$Volume,
       type = "h",
       xlab = "Date",
       ylab = "Volume",
       main = paste(symbol, "- Trading Volume"))
}

# 7. Run the analysis
portfolio_symbols <- read_portfolio()
print(portfolio_symbols)

stocks <- load_stock_data(
  from = Sys.Date() - 365,
  to = Sys.Date()
)

all_statistics <- list()

for (symbol in names(stocks)) {
  display_stock_data(symbol, stocks[[symbol]])

  stats <- calculate_statistics(stocks[[symbol]], ma_period = 20)
  all_statistics[[symbol]] <- stats
  display_statistics(symbol, stats)

  plot_stock_data(symbol, stocks[[symbol]], ma_period = 20)
}

cat("\nAssignment 2 analysis completed successfully.\n")
