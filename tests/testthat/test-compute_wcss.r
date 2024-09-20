library(testthat)

# Helper function to create mock data
create_mock_data <- function() {
  # Create a simple data frame with a numeric variable
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  return(data)
}

test_that("compute_wcss returns correct WCSS values for valid input", {
  data <- create_mock_data()
  k_values <- c(1, 2, 3)
  
  # Compute WCSS
  result <- compute_wcss(data, "x", k_values)
  
  # Check that the result length matches the number of k values
  expect_equal(length(result), length(k_values))
  
  # Check that WCSS values are non-negative
  expect_true(all(result >= 0))
})

create_mock_data <- function() {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  return(data)
}

test_that("compute_wcss handles edge cases gracefully", {
  data <- create_mock_data()
  
  # Case 1: k value greater than the number of data points
  k_values_too_large <- c(11, 12)
  expect_error(compute_wcss(data, "x", k_values_too_large), 
               "k values must be strictly less than the number of data points and positive.")
  
  # Case 2: k value equal to the number of data points
  k_values_equal_to_n <- nrow(data)
  expect_error(compute_wcss(data, "x", k_values_equal_to_n), 
               "k values must be strictly less than the number of data points and positive.")
  
  # Case 3: k values with negative or zero
  k_values_invalid <- c(1, -1, 0)
  expect_error(compute_wcss(data, "x", k_values_invalid), 
               "k_values must be a numeric vector with positive values.")
  
  # Case 4: Non-numeric k values
  k_values_non_numeric <- c("a", 2)
  expect_error(compute_wcss(data, "x", k_values_non_numeric), 
               "k_values must be a numeric vector with positive values.")
})




test_that("compute_wcss handles missing variables", {
  data <- create_mock_data()
  
  # Test with a variable that does not exist in the data
  expect_error(compute_wcss(data, "non_existent_variable", c(1, 2)),
               "Variable non_existent_variable does not exist in the data frame.")
})

test_that("compute_wcss handles non-numeric data", {
  data <- data.frame(
    x = c("a", "b", "c", "d", "e")
  )
  
  # Test with non-numeric data
  expect_error(compute_wcss(data, "x", c(1, 2)), 
               "Variable x is not numeric.")
})