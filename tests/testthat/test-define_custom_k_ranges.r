library(testthat)

# Example data frame for testing
species_names <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
FD_values <- runif(min = -2, max = 2, n = 23)
range_values <- runif(min = -2, max = 2, n = 23)
mean_evol_dist_values <- runif(min = -2, max = 2, n = 23)
forest_data <- data.frame(species_name = species_names, fun_dist = FD_values, range_size = range_values, mean_evol_dist = mean_evol_dist_values)

# Define test cases
test_that("define_custom_k_ranges performs clustering and defines custom ranges correctly", {
  # Test with k = 3
  result <- define_custom_k_ranges(data = forest_data, variable = "range_size", k = 3)
  
  # Check that the data has been updated with the Cluster column
  expect_true("Cluster" %in% colnames(result$data))
  expect_equal(length(result$data$Cluster), nrow(forest_data))
  
  # Check that custom_ranges has the correct number of ranges
  expect_length(result$custom_ranges, 3)
  
  # Check that custom ranges are numeric and have correct bounds
  for (i in seq_along(result$custom_ranges)) {
    expect_true(is.numeric(result$custom_ranges[[i]]))
    expect_length(result$custom_ranges[[i]], 2)
  }
  
  # Test with k = 5
  result <- define_custom_k_ranges(data = forest_data, variable = "fun_dist", k = 5)
  
  # Check that the data has been updated with the Cluster column
  expect_true("Cluster" %in% colnames(result$data))
  expect_equal(length(result$data$Cluster), nrow(forest_data))
  
  # Check that custom_ranges has the correct number of ranges
  expect_length(result$custom_ranges, 5)
  
  # Check that custom ranges are numeric and have correct bounds
  for (i in seq_along(result$custom_ranges)) {
    expect_true(is.numeric(result$custom_ranges[[i]]))
    expect_length(result$custom_ranges[[i]], 2)
  }
})

test_that("define_custom_k_ranges handles edge cases correctly", {
  # Test with k = 1 (edge case with only one cluster)
  result <- define_custom_k_ranges(data = forest_data, variable = "range_size", k = 1)
  
  # Check that the data has been updated with the Cluster column
  expect_true("Cluster" %in% colnames(result$data))
  expect_equal(length(result$data$Cluster), nrow(forest_data))
  
  # Check that custom_ranges has only one range
  expect_length(result$custom_ranges, 1)
  
  # Check that custom ranges are numeric and have correct bounds
  expect_true(is.numeric(result$custom_ranges[[1]]))
  expect_length(result$custom_ranges[[1]], 2)
  
  # Test with k greater than the number of observations (edge case)
  expect_error(
    define_custom_k_ranges(data = forest_data, variable = "mean_evol_dist", k = 30),
    regexp = "Number of clusters \\(k\\) cannot be greater than the number of observations in the data."
  )
})


test_that("define_custom_k_ranges fails with invalid inputs", {
  # Test with invalid variable name
  expect_error(
    define_custom_k_ranges(data = forest_data, variable = "nonexistent_var", k = 3),
    "The specified variable must be found within the data frame."
  )
  
  # Test with invalid k (negative number)
  expect_error(
    define_custom_k_ranges(data = forest_data, variable = "range_size", k = -3),
    "Number of clusters \\(k\\) must be a positive integer."
  )
  
  # Test with k greater than the number of observations
  expect_error(
    define_custom_k_ranges(data = forest_data, variable = "range_size", k = 30),
    "Number of clusters \\(k\\) cannot be greater than the number of observations in the data."
  )
})
