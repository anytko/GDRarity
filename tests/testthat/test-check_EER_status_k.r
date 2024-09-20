library(testthat)
library(dplyr)

# Mock the define_custom_k_ranges function for testing
mock_define_custom_k_ranges <- function(data, variable, k) {
  # Define custom ranges based on the variable and k
  if (variable == "range_size") {
    return(list(custom_ranges = list("Cluster_1" = c(0, 10), "Cluster_2" = c(10, Inf))))
  } else if (variable == "mean_evol_dist") {
    return(list(custom_ranges = list("Cluster_1" = c(0, 3), "Cluster_2" = c(3, Inf))))
  } else if (variable == "fun_dist") {
    return(list(custom_ranges = list("Cluster_1" = c(-Inf, 1), "Cluster_2" = c(1, Inf))))
  }
}

# Simulate a data frame for testing
mock_data <- data.frame(
  species_name = c("Species1", "Species2", "Species3", "Species4", "Species5"),
  range_size = c(10, 20, 15, 5, 25),
  mean_evol_dist = c(2.5, 4.0, 3.5, 6.0, 3.0),
  fun_dist = c(1.0, 1.5, 0.5, 2.0, 1.2)
)

# Test cases for the function
test_that("check_EER_status_k categorizes species correctly", {
  result <- check_EER_status_k(
    data_frame = mock_data,
    range_size_col = "range_size",
    mean_evol_dist_col = "mean_evol_dist",
    fun_dist_col = "fun_dist",
    range_size_k = 2,
    mean_evol_dist_k = 2,
    fun_dist_k = 2
  )

  # Expected classifications based on mock data and defined thresholds
  expected_classifications <- c(
    "Endemic",    # Species1: range_size <= threshold, evol_dist <= threshold, trait_condition
    "Indicator", # Species2: range_size > threshold, evol_dist > threshold, trait_condition
    "Adaptable Survivor", # Species3: range_size > threshold, evol_dist <= threshold, !trait_condition
    "Classically Rare",     # Species4: range_size <= threshold, evol_dist > threshold, !trait_condition
    "High Invasive Potential"      # Species5: range_size > threshold, evol_dist <= threshold, trait_condition
  )

  expect_equal(result$classifications, expected_classifications)
})

test_that("check_EER_status_k handles missing values gracefully", {
  # Add missing values to the mock data
  mock_data_with_na <- mock_data
  mock_data_with_na$fun_dist[2] <- NA

  result <- check_EER_status_k(
    data_frame = mock_data_with_na,
    range_size_col = "range_size",
    mean_evol_dist_col = "mean_evol_dist",
    fun_dist_col = "fun_dist",
    range_size_k = 2,
    mean_evol_dist_k = 2,
    fun_dist_k = 2
  )

  # Expected classifications with NA handling
  expected_classifications_with_na <- c(
    "Endemic",
    "NA",
    "Adaptable Survivor",
    "Classically Rare",
    "High Invasive Potential"
  )

  expect_equal(result$classifications, expected_classifications_with_na)
})

test_that("check_EER_status_k returns NA classification if all data is missing", {
  # Create a data frame with all NA values
  all_na_data <- data.frame(
    species_name = c("Species6"),
    range_size = NA,
    mean_evol_dist = NA,
    fun_dist = NA,
    classifications = NA_character_
  )


expect_error(
  check_EER_status_k(
    data_frame = all_na_data,
    range_size_col = "range_size",
    mean_evol_dist_col = "mean_evol_dist",
    fun_dist_col = "fun_dist",
    range_size_k = 2,
    mean_evol_dist_k = 2,
    fun_dist_k = 2
  ),
  "All values in one or more columns are NA. Cannot perform classification."
)
})

