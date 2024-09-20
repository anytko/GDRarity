library(testthat)
library(ape)
library(picante)
library(phytools)
library(doParallel)
library(foreach)

# Helper function to create a sample phylogenetic tree
create_sample_tree <- function() {
  tree <- rtree(10)
  tree$edge.length <- abs(tree$edge.length) # Ensure positive branch lengths
  return(tree)
}

# Helper function to create a sample data frame
create_sample_data <- function(taxa) {
  data.frame(species_name = taxa)
}

# Tests for avg_evol_dist function
test_that("avg_evol_dist calculates average evolutionary distinctiveness correctly", {
  # Create a sample phylogenetic tree and data frame
  sample_tree <- create_sample_tree()
  sample_data <- create_sample_data(sample_tree$tip.label)

  
  # Run the avg_evol_dist function
  result <- avg_evol_dist(
    phy = sample_tree,
    data_frame = sample_data,
    time_slices = c(0.1, 0.12, 0.15),
    num_cores = 1
  )

  class(sample_tree)
  
  # Verify that result is a data frame
  expect_is(result, "data.frame")
  
  # Verify the structure of the result
  expect_true(all(c("species_name", "mean_evol_dist") %in% colnames(result)))
  
  # Verify that species names in the result are from the input species_data
  expect_true(all(result$species_name %in% sample_data$species_name))
  
  # Verify that mean_evol_dist is numeric or NA
  expect_true(all(is.na(result$mean_evol_dist) | is.numeric(result$mean_evol_dist)))
})

test_that("branching.times works with a simple phylo object", {
  # Create a valid phylo tree
  sample_tree <- ape::rtree(10)
  
  # Verify that it's a 'phylo' object
  expect_true(inherits(sample_tree, "phylo"))
  
  # Call branching.times
  times <- ape::branching.times(sample_tree)
  
  # Check the output
  expect_true(is.numeric(times))
})

  
test_that("avg_evol_dist handles empty species data frame", {
  # Create a valid phylo tree
  sample_tree <- create_sample_tree()  # Generates a phylogenetic tree with 10 tips
  
  # Check if sample_tree is of class 'phylo'
  expect_s3_class(sample_tree, "phylo")
  
  # Create an empty data frame
  empty_data <- data.frame()
  
  # Expect an error when passing an empty data frame
  expect_error(
    avg_evol_dist(
      phy = sample_tree,
      data_frame = empty_data,
      time_slices = c(0.01, 0.012, 0.015),
      num_cores = 1
    ),
    "The provided data frame is empty. Cannot compute evolutionary distinctiveness."
  )
})

test_that("avg_evol_dist handles invalid num_cores", {
  # Create a sample phylogenetic tree and data frame
  sample_tree <- create_sample_tree()
  sample_data <- create_sample_data(sample_tree$tip.label)
  
  # Expect an error if num_cores is less than 1
  expect_error(
    avg_evol_dist(
      phy = sample_tree,
      data_frame = sample_data,
      num_cores = 0
    ),
    "num_cores must be at least 1"
  )
})

test_that("avg_evol_dist throws an error for time slices outside phylogeny range", {
  # Create a sample phylogenetic tree and data frame
  sample_tree <- create_sample_tree()
  sample_data <- create_sample_data(sample_tree$tip.label)
  
  # Expect an error if time slices are greater than the root age
  expect_error(
    avg_evol_dist(
      phy = sample_tree,
      data_frame = sample_data,
      time_slices = c(150),
      num_cores = 1
    ),
    "Chosen time slices are not included within the phylogeny"
  )
})

test_that("avg_evol_dist processes with multiple cores", {
  # Create a sample phylogenetic tree and data frame
  sample_tree <- create_sample_tree()
  sample_data <- create_sample_data(sample_tree$tip.label)
  
  # Use a small number of cores (e.g., 2) for testing
  result <- avg_evol_dist(
    phy = sample_tree,
    data_frame = sample_data,
    time_slices = c(0.01, 0.015),
    num_cores = 2
  )
  
  # Verify that result is a data frame
  expect_is(result, "data.frame")
  
  # Verify the structure of the result
  expect_true(all(c("species_name", "mean_evol_dist") %in% colnames(result)))
  
  # Verify that mean_evol_dist is numeric or NA
  expect_true(all(is.na(result$mean_evol_dist) | is.numeric(result$mean_evol_dist)))
})

test_that("avg_evol_dist handles missing species gracefully", {
  # Create a sample phylogenetic tree with different tips
  sample_tree <- create_sample_tree()
  missing_species <- setdiff(sample_tree$tip.label, paste0("species_", 1:5))
  sample_data <- create_sample_data(missing_species)
  
  # Run the avg_evol_dist function
  result <- avg_evol_dist(
    phy = sample_tree,
    data_frame = sample_data,
    time_slices = c(0.01, 0.015),
    num_cores = 1
  )
  
  # Verify that the result is a data frame
  expect_is(result, "data.frame")
  
  # Verify that the result contains the missing species
  expect_true(all(result$species_name %in% missing_species))
  
  # Verify that mean_evol_dist is numeric or NA
  expect_true(all(is.na(result$mean_evol_dist) | is.numeric(result$mean_evol_dist)))
})



