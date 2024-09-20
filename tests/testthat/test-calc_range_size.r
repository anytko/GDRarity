library(testthat)
library(future)
library(sf)

# Create a mock data frame for testing
mock_data <- data.frame(
  species_name = c("Species A", "Species B", "Species C"),
  stringsAsFactors = FALSE
)

# Mock functions to replace actual implementations
get_range_convex_hulls <- function(data_frame, species_name = NULL, num_cores = 1, min_points = 5, min_distance = 1, gbif_limit = 2000) {
  # Define a base point for simplicity
  base_point <- st_point(c(0, 0))
  
  if (is.null(species_name)) {
    species_names <- data_frame$species_name
  } else {
    species_names <- species_name
  }
  
  # Generate a convex hull for each species with a unique buffer size
  convex_hulls_results <- lapply(species_names, function(name) {
    # Create a convex hull with a buffer of varying size for each species
    buffer_size <- match(name, species_names) * 0.5  # Change buffer size for each species
    message("Generating convex hull for species:", name)
    
    # Create a buffer around the base point and convert it to an `sf` object
    buffer_polygon <- st_buffer(base_point, buffer_size)
    convex_hull_sf <- st_as_sf(st_sfc(buffer_polygon, crs = 4326))
    
    return(convex_hull_sf)
  })
  
  # Combine the results into a single list
  names(convex_hulls_results) <- species_names
  return(convex_hulls_results)
}

clip_polygons_to_land <- function(convex_hulls, continent_sf) {
  # Simulate output of this function
  return(convex_hulls)
}

range_sizes <- function(clipped_polygons_list) {
  # Extract species names from the list
  species_names <- names(clipped_polygons_list)
  
  # Debugging output
  print(paste("Species names:", paste(species_names, collapse = ", ")))
  
  # Assign a fixed range size of 10 to all species
  simulated_sizes <- data.frame(
    species_name = species_names,
    range_size = rep(10, length(species_names))  # All species get a range size of 10
  )
  
  # Debugging output
  print("Simulated sizes:")
  print(simulated_sizes)
  
  return(simulated_sizes)
}

test_that("calc_range_size handles various inputs correctly", {
  # Test with default parameters
  result <- calc_range_size(mock_data, num_cores = 1)
  expect_equal(nrow(result), 3)  # Check for all species
  expect_setequal(result$species_name, mock_data$species_name)
  
  # Check range sizes match the expected ones
  expect_equal(result$range_size[match("Species A", result$species_name)], 10)
  expect_equal(result$range_size[match("Species B", result$species_name)], 10)
  expect_equal(result$range_size[match("Species C", result$species_name)], 10)
  
  # Test with a specific continent file
  tmp_continent_file <- tempfile(fileext = ".geojson")
  polygon <- st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))))
  sfc_polygon <- st_sfc(polygon, crs = 4326)
  continent_sf <- st_sf(geometry = sfc_polygon)
  st_write(continent_sf, tmp_continent_file)
  
  result_with_file <- calc_range_size(mock_data, num_cores = 1, continent_file = tmp_continent_file)
  expect_equal(nrow(result_with_file), 3)  # Check for all species
  expect_setequal(result_with_file$species_name, mock_data$species_name)
  
  # Check range sizes match the expected ones
  expect_equal(result_with_file$range_size[match("Species A", result_with_file$species_name)], 10)
  expect_equal(result_with_file$range_size[match("Species B", result_with_file$species_name)], 10)
  expect_equal(result_with_file$range_size[match("Species C", result_with_file$species_name)], 10)
  
  # Test with no continent file (uses default)
  result_no_file <- calc_range_size(mock_data, num_cores = 1, continent_file = NULL)
  expect_equal(nrow(result_no_file), 3)  # Check for all species
  expect_setequal(result_no_file$species_name, mock_data$species_name)
  
  # Check range sizes match the expected ones
  expect_equal(result_no_file$range_size[match("Species A", result_no_file$species_name)], 10)
  expect_equal(result_no_file$range_size[match("Species B", result_no_file$species_name)], 10)
  expect_equal(result_no_file$range_size[match("Species C", result_no_file$species_name)], 10)
  
  # Clean up temporary files
  unlink(tmp_continent_file)
})

test_that("calc_range_size handles edge cases correctly", {
  # Edge case: Empty data frame
  empty_data <- data.frame(species_name = character())
  result_empty <- calc_range_size(empty_data, num_cores = 1)
  expect_equal(nrow(result_empty), NULL)
  
  # Edge case: Single species
  single_species_data <- data.frame(species_name = "Species X")
  result_single_species <- calc_range_size(single_species_data, num_cores = 1)
  expect_equal(nrow(result_single_species), 1)
  expect_equal(result_single_species$species_name, "Species X")
  expect_equal(result_single_species$range_size, 10)
})
