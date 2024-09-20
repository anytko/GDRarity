library(testthat)
library(sf)

# Mock or Sample Data
sample_data <- data.frame(species_name = c("Eucalyptus globulus"), stringsAsFactors = FALSE)
mock_continent_sf <- st_as_sf(st_sfc(st_polygon(list(rbind(c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90))))))
# Define the clip_polygons_to_land and plot_clipped_hulls functions if they are not yet defined

# Test for valid input with clipping
test_that("range_poly_map works with clipping", {
  # Mocking the functions used inside
  mock_get_range_convex_hulls <- function(...) list(Species_A = list(st_as_sf(st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))))))
  mock_plot_clipped_hulls <- function(x) {
    expect_is(x, "leaflet")
    expect_true(length(x$x$groups) > 0)  # Check that the map has groups (polygons)
  }
  
  result <- range_poly_map(
    data_frame = sample_data,
    species_name = "Eucalyptus globulus",
    clip = TRUE,
    continent_sf = mock_continent_sf,
    plot = TRUE
  )
  
  expect_s3_class(result, "leaflet")
})

# Test for valid input without clipping
test_that("range_poly_map works without clipping", {
  # Mocking the functions used inside
  mock_get_range_convex_hulls <- function(...) list(Species_A = list(st_as_sf(st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))))))
  mock_plot_convex_hulls <- function(x) {
    expect_is(x, "leaflet")
    expect_true(length(x$x$groups) > 0)  # Check that the map has groups (polygons)
  }
  
  result <- range_poly_map(
    data_frame = sample_data,
    species_name = "Eucalyptus globulus",
    clip = FALSE,
    plot = TRUE
  )
  
  expect_s3_class(result, "leaflet")
})

# Test for error handling for missing continent_sf
test_that("range_poly_map stops with missing continent_sf when clipping", {
  expect_error(
    range_poly_map(
      data_frame = sample_data,
      species_name = "Eucalyptus globulus",
      clip = TRUE,
      plot = TRUE
    ),
    "continent_sf must be provided when clip = TRUE"
  )
})

# Test for no plotting
test_that("range_poly_map handles no plotting", {
  # Mocking the functions used inside
  mock_get_range_convex_hulls <- function(...) list(Species_A = list(st_as_sf(st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))))))
  mock_plot_convex_hulls <- function(x) {
    expect_true(FALSE, "Plotting should not occur")
  }
  
  result <- range_poly_map(
    data_frame = sample_data,
    species_name = "Eucalyptus globulus",
    plot = FALSE
  )
  
  expect_null(result)  # Ensure result is NULL when not plotting
})

