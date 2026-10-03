# BDA400 Assignment 5 - Crossover
# Follows the assignment pseudocode and returns "Up", "Down", or "None".

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (length(arr1) == 0) return(character(0))

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
