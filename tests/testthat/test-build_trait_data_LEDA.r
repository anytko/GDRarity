library(testthat)
library(dplyr)
library(purrr)
library(stringr)


# Test 1: Test with default parameters (no genera provided)
test_that("Function works with default parameters", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "seed_mass"))
  expect_true("species_name" %in% colnames(result))
  expect_true("SLA" %in% colnames(result))
  expect_true("seed_mass" %in% colnames(result))
  expect_true(nrow(result) > 0)
})

# Test 2: Test with specific genera filter
test_that("Function works with specific genera", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "canopy_height"), genera = c("Eucalyptus_", "Quercus_"))
  expect_true("species_name" %in% colnames(result))
  expect_true("SLA" %in% colnames(result))
  expect_true("canopy_height" %in% colnames(result))
  
  # Check that species names correspond to the selected genera
  genera_species <- unique(result$species_name)
  expect_true(all(grepl("Eucalyptus|Quercus", genera_species)))
})

# Test 3: Test with invalid column names
test_that("Function errors with invalid column names", {
  expect_error(build_trait_data_LEDA(columns_to_select = c("non_existent_column")),
               "Invalid columns selected: non_existent_column")
})

# Test 4: Test with valid and invalid column names
test_that("Function errors with mixed valid and invalid column names", {
  expect_error(build_trait_data_LEDA(columns_to_select = c("SLA", "non_existent_column")),
               "Invalid columns selected: non_existent_column")
})

# Test 5: Test output structure and column names
test_that("Function outputs the correct structure and columns", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "leaf_mass"), genera = c("Eucalyptus_", "Quercus_"))
  expect_true("species_name" %in% colnames(result))
  expect_true("SLA" %in% colnames(result))
  expect_true("leaf_mass" %in% colnames(result))
  expect_true(nrow(result) > 0)
  expect_true(is.data.frame(result))
})

# Test 6: Test that species names are correctly formatted
test_that("Species names are correctly formatted", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "leaf_mass"), genera = c("Eucalyptus_", "Quercus_"))
  expect_true(all(!grepl(" ", result$species_name)))  # Species names should have no spaces
})

# Test 7: Test that function works without filtering by genera
test_that("Function works without genera filter", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "seed_mass"))
  expect_true("species_name" %in% colnames(result))
  expect_true(nrow(result) > 0)  # Should return rows for all species
})

# Test 8: Test with no rows in result due to genera mismatch
test_that("No species are returned if no match with provided genera", {
  result <- build_trait_data_LEDA(columns_to_select = c("SLA", "leaf_mass"), genera = c("InvalidGenus"))
  expect_equal(nrow(result), 0)  # No species should match invalid genera
})

