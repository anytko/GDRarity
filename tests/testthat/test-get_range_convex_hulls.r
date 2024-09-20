library(testthat)
library(sf)
library(dplyr)
library(dbscan)
library(rgbif)
library(mockery) 

mock_data <- data.frame(
  species_name = c("Species_One", "Species_Two"),
  stringsAsFactors = FALSE
)

# Mock GBIF occurrence search result with lat/long coordinates, countryCode, and basisOfRecord
mock_gbif_data <- list(
  data = data.frame(
    decimalLatitude = c(-34.9285, -35.0, -35.1),
    decimalLongitude = c(138.6007, 138.7, 138.8),
    occurrenceID = c("ID1", "ID2", "ID3"),
    countryCode = c("AU", "AU", "AU"),  # Add countryCode for Australia
    basisOfRecord = c("HUMAN_OBSERVATION", "HUMAN_OBSERVATION", "HUMAN_OBSERVATION"),  # Add basisOfRecord
    stringsAsFactors = FALSE
  )
)

# Mock the rgbif::occ_search function to return the mock GBIF data
mock_occ_search <- function(scientificName, hasCoordinate, limit) {
  return(mock_gbif_data)
}

test_that("get_range_convex_hulls handles valid parameters correctly", {
  # Mock rgbif::occ_search within this test
  stub(get_range_convex_hulls, 'rgbif::occ_search', mock_occ_search)
  
  result <- get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", num_cores = 1, min_points = 1)
  
  expect_type(result, "list")
  expect_true("Species_One" %in% names(result))
  expect_length(result[["Species_One"]], 1)
})

test_that("get_range_convex_hulls handles parallel processing correctly", {
  # Mock rgbif::occ_search within this test
  stub(get_range_convex_hulls, 'rgbif::occ_search', mock_occ_search)
  
  result_parallel <- get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", num_cores = 3, min_points = 1)
  
  expect_type(result_parallel, "list")
  expect_true("1.Species_One" %in% names(result_parallel))
  expect_length(result_parallel[["1.Species_One"]], 1)
})

test_that("get_range_convex_hulls handles NULL species_name correctly", {
  # Mock rgbif::occ_search within this test
  stub(get_range_convex_hulls, 'rgbif::occ_search', mock_occ_search)
  
  result_no_species_name <- get_range_convex_hulls(data_frame = mock_data, num_cores = 1, min_points=1)
  
  expect_type(result_no_species_name, "list")
  expect_true("Species_One" %in% names(result_no_species_name))
  expect_true("Species_Two" %in% names(result_no_species_name))
})

test_that("get_range_convex_hulls returns error for invalid num_cores", {
  expect_error(
    get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", num_cores = -1),
    "num_cores must be a positive integer"
  )
})

test_that("get_range_convex_hulls returns error for invalid min_points", {
  expect_error(
    get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", min_points = -1),
    "min_points must be a positive whole number"
  )
})

test_that("get_range_convex_hulls returns error for invalid min_distance", {
  expect_error(
    get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", min_distance = -1),
    "min_distance must be a positive number"
  )
})

test_that("get_range_convex_hulls handles gbif_limit of 0 correctly", {
  # Mock rgbif::occ_search within this test
  stub(get_range_convex_hulls, 'rgbif::occ_search', mock_occ_search)
  
  result_gbif_limit <- get_range_convex_hulls(data_frame = mock_data, species_name = "Species_One", gbif_limit = 0)
  
  expect_type(result_gbif_limit, "list")
  expect_length(result_gbif_limit[["Species_One"]], 0)
})

test_that("get_range_convex_hulls handles no GBIF data correctly", {
  # Mock rgbif::occ_search within this test
  stub(get_range_convex_hulls, 'rgbif::occ_search', mock_occ_search)
  
  # Capture the output
  output <- capture.output({
    result_no_data <- get_range_convex_hulls(data_frame = mock_data, species_name = "Nonexistent_Species", num_cores = 1)
  })
  
  # Ensure the result is a list and is empty
  expect_type(result_no_data, "list")
  
  # Ensure the output is character(0)
  expect_equal(output, character(0))
})




