library(testthat)
library(sf)

create_example_data <- function() {
  # Create example continent polygons with a specified CRS
  continent_polygons <- list(
    st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))))
  )
  continent_sf <- st_as_sf(data.frame(
    continent = c("Africa", "Asia"),
    geometry = st_sfc(continent_polygons)
  ), crs = 4326)  # Set CRS to WGS84
  
  # Create example convex hulls for species with the same CRS
  convex_hulls <- list(
    species1 = list(
      st_polygon(list(rbind(c(0, 0), c(0.5, 0), c(0.5, 0.5), c(0, 0.5), c(0, 0)))),
      st_polygon(list(rbind(c(1, 1), c(1.5, 1), c(1.5, 1.5), c(1, 1.5), c(1, 1)))),
      st_polygon(list(rbind(c(0.2, 0.2), c(0.8, 0.2), c(0.8, 0.8), c(0.2, 0.8), c(0.2, 0.2)))),
      st_polygon(list(rbind(c(1.2, 1.2), c(1.8, 1.2), c(1.8, 1.8), c(1.2, 1.8), c(1.2, 1.2)))),
      st_polygon(list(rbind(c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5)))),
      st_polygon(list(rbind(c(1.5, 1.5), c(2, 1.5), c(2, 2), c(1.5, 2), c(1.5, 1.5)))),
      st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))))
    ),
    species2 = list(
      st_polygon(list(rbind(c(0.5, 0.5), c(1, 0.5), c(1, 1), c(0.5, 1), c(0.5, 0.5))))
    )
  )
  
  list(continent_sf = continent_sf, convex_hulls = convex_hulls)
}


test_that("check_continents function works correctly", {
  data <- create_example_data()
  continent_sf <- data$continent_sf
  convex_hulls <- data$convex_hulls

  result <- check_continents(convex_hulls, continent_sf)
  
  # Check that result is a dataframe
  expect_s3_class(result, "data.frame")

  
  # Check that result contains the correct columns
    expected_columns <- c("Africa", "Asia", "species_name")
    expect_true(all(expected_columns %in% colnames(result)))
  
  # Check that the species names are in the result
  expect_true("species1" %in% result$species_name)
  expect_true("species2" %in% result$species_name)
  
  # Check specific values in the result
  species1_row <- result[result$species_name == "species1", ]
  expect_equal(species1_row$Africa, 1)  # Expecting 1 for Africa due to overlap
  expect_equal(species1_row$Asia, 1)    # Expecting 1 for Asia due to overlap

  species2_row <- result[result$species_name == "species2", ]
  expect_equal(species2_row$Africa, 1)  # Expecting 1 for Africa due to overlap
  expect_equal(species2_row$Asia, 1)    # Expecting 1 for Asia due to overlap
})

test_that("check_continents handles empty inputs", {
  empty_convex_hulls <- list()
  data <- create_example_data()
  continent_sf <- data$continent_sf

  expect_error(check_continents(empty_convex_hulls, continent_sf), 
               "Error: The convex_hulls list is empty. Please provide valid convex hulls.")
})

test_that("check_continents handles missing continents gracefully", {
  data <- create_example_data()
  
  # Test missing continent_sf
  expect_error(check_continents(data$convex_hulls),
               "Error: The continent_sf input is missing. Please provide a valid sf object with geometries.")
  
  # Test continent_sf as a non-sf object (e.g., data.frame)
  continent_sf <- st_drop_geometry(data$continent_sf)
  convex_hulls <- data$convex_hulls
  
  # Expect an error because continent_sf is a data.frame, not an sf object
  expect_error(check_continents(convex_hulls, continent_sf),
               "Error: The continent_sf input is not an sf object. Please provide a valid sf object with geometries.")
})

