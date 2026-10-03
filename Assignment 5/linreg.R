# BDA400 Assignment 5 - Linear Regression (linreg)
# Uses the assignment template/pseudocode and only base R.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || length(regressionSource) == 0) {
    stop("regressionSource must be a non-empty numeric vector")
  }
  if (regressionLength < 2 || regressionLength != as.integer(regressionLength)) {
    stop("regressionLength must be an integer of at least 2")
  }
  if (regressionOffset < 0 || regressionOffset != as.integer(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }

  # Calculate the total number of elements in regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate starting and ending indexes
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)

  # Calculate means
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)

  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  if (denominator == 0) stop("Cannot calculate regression because denominator is zero")

  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate predicted values
  predicted_values <- slope * index_values + intercept

  # Return results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}
