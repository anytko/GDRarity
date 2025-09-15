library(testthat)
library(mockery)
library(sf)
library(dbscan)

test_that("get_range_convex_hulls works on mocked data", {
  # Mock occurrence data with coordinates that cluster nicely
  mock_occ_data <- data.frame(
    occurrenceID = 1:10,
    decimalLatitude = c(37, 37.1, 37.2, 37.3, 37.35, 36.5, 36.55, 36.6, 36.65, 36.7),
    decimalLongitude = c(-115.7, -115.68, -115.66, -115.64, -115.62, -115.8, -115.78, -115.76, -115.74, -115.72),
    countryCode = rep("US", 10),
    basisOfRecord = rep("HUMAN_OBSERVATION", 10)
  )
  
  mock_gbif_result <- list(data = mock_occ_data)
  
  # Mock rgbif::occ_search to always return the mock_gbif_result
  mock_occ_search <- mockery::mock(mock_gbif_result)
  mockery::stub(get_range_convex_hulls, "rgbif::occ_search", mock_occ_search)
  
  test_df <- data.frame(species_name = "Fake_species")
  
  result <- get_range_convex_hulls(test_df, num_cores = 1, min_points = 2, min_distance = 0.2)
  
  # Check that result is a list with one element named exactly "Fake_species"
  expect_type(result, "list")
  expect_named(result, "Fake_species")
  
  # The element for Fake_species should be a non-empty list (clusters)
  expect_true(is.list(result$Fake_species))
  expect_gt(length(result$Fake_species), 0)
  
  # Each element in the list should be an sf geometry of polygon or multipolygon
  all_polygons <- all(sapply(result$Fake_species, function(polygon_geom) {
    inherits(polygon_geom, "sfc_POLYGON") || inherits(polygon_geom, "sfc_MULTIPOLYGON")
  }))
  expect_true(all_polygons)
})


