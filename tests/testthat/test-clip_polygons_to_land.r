library(testthat)
library(sf)
library(rmapshaper)

# Create a valid mock convex hull
create_mock_convex_hulls <- function() {
  # Define a simple polygon that is properly closed
  polygon <- matrix(c(
    0, 0,
    1, 0,
    1, 1,
    0, 1,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  # Create an sf polygon
  sf_polygon <- st_polygon(list(polygon))
  st_sf(geometry = st_sfc(sf_polygon))
}

# Create a valid mock continent_sf
create_mock_continent_sf <- function() {
  # Define a simple polygon that is properly closed
  polygon <- matrix(c(
    -1, -1,
    2, -1,
    2, 2,
    -1, 2,
    -1, -1
  ), ncol = 2, byrow = TRUE)
  
  # Create an sf polygon
  sf_polygon <- st_polygon(list(polygon))
  st_sf(geometry = st_sfc(sf_polygon))
}

# Tests
test_that("clip_polygons_to_land handles valid inputs", {
  convex_hulls <- list(create_mock_convex_hulls())
  continent_sf <- create_mock_continent_sf()
  
  result <- clip_polygons_to_land(convex_hulls, continent_sf)
  
  expect_true(is.list(result))
  expect_length(result, length(convex_hulls))
})

test_that("clip_polygons_to_land handles NULL convex_hulls input", {
  expect_null(clip_polygons_to_land(NULL, create_mock_continent_sf()))
})

test_that("clip_polygons_to_land handles empty convex_hulls input", {
  expect_null(clip_polygons_to_land(list(), create_mock_continent_sf()))
})

test_that("clip_polygons_to_land handles convex_hulls with no intersection with continent_sf", {
  # Create a polygon that does not intersect with the continent polygon
  disjoint_polygon <- matrix(c(
    10, 10,
    11, 10,
    11, 11,
    10, 11,
    10, 10
  ), ncol = 2, byrow = TRUE)
  
  convex_hulls <- list(st_sf(geometry = st_sfc(st_polygon(list(disjoint_polygon)))))
  continent_sf <- create_mock_continent_sf()
  
  result <- clip_polygons_to_land(convex_hulls, continent_sf)
  
  # Since there's no intersection, the result should be an empty list
  expect_length(result[[1]], 0)
})
