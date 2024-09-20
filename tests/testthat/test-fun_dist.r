library(testthat)
library(vegan)

# Test the fun_dist function
test_that("fun_dist returns a data frame", {
  # Create example data for testing
  maple_data <- data.frame(
    species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", 
                     "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", 
                     "Acer saccharinum"),
    SLA = c(5, 15, 6, 11, 23.5, 52, 4),
    height = c(50, 65, 28, 70, 68, 43, 55),
    seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9)
  )
  
  result <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
  expect_s3_class(result, "data.frame")
})

test_that("fun_dist produces the correct number of rows and columns", {
  # Create example data for testing
  maple_data <- data.frame(
    species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", 
                     "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", 
                     "Acer saccharinum"),
    SLA = c(5, 15, 6, 11, 23.5, 52, 4),
    height = c(50, 65, 28, 70, 68, 43, 55),
    seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9)
  )
  
  result <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
  expect_equal(nrow(result), nrow(maple_data))
  expect_equal(ncol(result), ncol(maple_data) + 1)  # original columns + fun_dist column
})

test_that("fun_dist returns correct species names", {
  # Create example data for testing
  maple_data <- data.frame(
    species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", 
                     "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", 
                     "Acer saccharinum"),
    SLA = c(5, 15, 6, 11, 23.5, 52, 4),
    height = c(50, 65, 28, 70, 68, 43, 55),
    seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9)
  )
  
  result <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
  expect_equal(result$species_name, maple_data$species_name)
})

library(testthat)

test_that("fun_dist calculates functional distinctiveness correctly", {
  # Sample data
  maple_data <- data.frame(
    species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", "Acer saccharinum"),
    SLA = c(5, 15, 6, 11, 23.5, 52, 4),
    height = c(50, 65, 28, 70, 68, 43, 55),
    seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9)
  )
  
  # Run the function
  result <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
  
  # Expected functional distinctiveness values (match with result)
  expected_fd <- c(1.45788106, 1.51670174, 2.13684282, 2.63543333, 1.66531801, 2.38919323, 1.42815126)
  
  # Compare without considering names
  expect_equal(result$fun_dist, expected_fd)
})


test_that("fun_dist handles edge cases correctly", {
  # Edge case: Only one trait
  maple_data_one_trait <- data.frame(
    species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", 
                     "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", 
                     "Acer saccharinum"),
    SLA = c(5, 15, 6, 11, 23.5, 52, 4)
  )
  expect_error(fun_dist(data_frame = maple_data_one_trait, trait_columns = c("SLA")), 
               "At least 2 traits are required for functional distinctiveness calculation.")

  # Edge case: No data
  empty_data <- data.frame(species_name = character(), SLA = numeric(), height = numeric(), seed_mass = numeric())
  expect_error(fun_dist(data_frame = empty_data, trait_columns = c("SLA", "height", "seed_mass")),
               "The data frame is empty.")
})



