# BDA400 Assignment 5 - Manual Test Runner
# Run this file in RStudio after setting the working directory to R_scripts.

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("\n--- SMA ---\n")
data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(sma(data1, 3))

cat("\n--- EMA ---\n")
print(ema(data1, 3))

cat("\n--- MACD ---\n")
data2 <- c(100, 105, 110, 115, 120, 125, 130)
print(macd(data2, 3, 5, 2))

cat("\n--- Standard Deviation ---\n")
print(stdev(data1))

cat("\n--- Linear Regression ---\n")
print(linreg(data1, 5, 0))

cat("\n--- RSI ---\n")
data3 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 66, 68, 64, 70, 72, 69, 75, 77, 73, 80)
print(rsi(data3, 5))

cat("\n--- Stochastic RSI ---\n")
print(stoch_rsi(data3, 5, 3, 3))

cat("\n--- Crossover ---\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
print(crossover(arr1, arr2))

cat("\n--- Crossunder ---\n")
print(crossunder(arr1, arr2))

cat("\nAll nine functions executed. Review the console output and take screenshots for your submission/repository evidence.\n")
