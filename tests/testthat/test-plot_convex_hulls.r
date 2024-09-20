library(testthat)
library(leaflet)
library(sf)
library(RColorBrewer)

# Define the test cases

convex_hulls <- list(
  "Species_A" = list(
    st_sfc(st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))), crs = 4326)
  ),
  "Species_B" = list(
    st_sfc(st_polygon(list(matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE))), crs = 4326)
  )
)

test_that("plot_convex_hulls handles empty input", {
  # Test case 1: Empty list input
  empty_input <- list()
  expect_silent(plot_convex_hulls(empty_input))
})

test_that("plot_convex_hulls handles valid input with multiple species", {
  
  # Call the function and suppress warnings
  map <- suppressWarnings(plot_convex_hulls(convex_hulls))
  
  # Expect the result to be a leaflet map
  expect_s3_class(map, "leaflet")
  
  # Check that polygons were added to the map
  expect_true(length(map$x$calls) > 1)  # There should be more than just the addTiles call
})

test_that("plot_convex_hulls generates a map with proper colors for multiple species", {
  # Create mock data for multiple species
  clipped_convex_hulls <- list(
    Species_A = list(st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(0,0)))))),
    Species_B = list(st_sfc(st_polygon(list(rbind(c(1,1), c(1,2), c(2,2), c(1,1))))))
  )
  
  # Call the function and suppress warnings
  map <- suppressWarnings(plot_convex_hulls(clipped_convex_hulls))
  
  # Expect the result to be a leaflet map
  expect_s3_class(map, "leaflet")
  
  # Find the addLegend call within the map's calls
  add_legend_call <- NULL
  for (call in map$x$calls) {
    if (call$method == "addLegend") {
      add_legend_call <- call
      break
    }
  }
  
  # Check if the legend was added
  expect_true(!is.null(add_legend_call), "The legend was not added to the map")
  
  # Check that legend labels are correct
  legend_labels <- as.character(add_legend_call$args[[1]]$labels)
  expect_equal(legend_labels, c("Species_A", "Species_B"))
})

test_that("plot_convex_hulls handles NULL geometries correctly", {
  # Create mock data with NULL geometries
  clipped_convex_hulls <- list(
    Species_A = list(NULL),
    Species_B = list(st_sfc(st_polygon(list(rbind(c(1,1), c(1,2), c(2,2), c(1,1))))))
  )
  
  # Call the function and suppress warnings
  map <- suppressWarnings(plot_convex_hulls(clipped_convex_hulls))
  
  # Expect the result to be a leaflet map
  expect_s3_class(map, "leaflet")
  
  # Check that polygons were added to the map (only for Species_B)
  expect_true(length(map$x$calls) > 1)  # There should be more than just the addTiles call
})
