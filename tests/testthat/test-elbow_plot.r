library(testthat)

set.seed(123)
species_names <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", 
                   "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", 
                   "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", 
                   "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", 
                   "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")

FD_values <- runif(23, -2, 2)
range_values <- runif(23, -2, 2)
evol_dist_values <- runif(23, -2, 2)

forest_data <- data.frame(
  species_name = species_names,
  fun_dist = FD_values,
  range_size = range_values,
  mean_evol_dist = evol_dist_values
)

test_that("elbow_plot generates a plot without errors", {
  expect_silent({
    # Save plot to a temporary file
    png_file <- tempfile(fileext = ".png")
    png(filename = png_file)
    elbow_plot(forest_data, "range_size")
    dev.off()
    
    # Check if the file is created
    expect_true(file.exists(png_file))
    
    # Clean up
    unlink(png_file)
  })
})

test_that("elbow_plot handles different k_max values", {
  expect_silent({
    # Save plot to a temporary file
    png_file <- tempfile(fileext = ".png")
    png(filename = png_file)
    elbow_plot(forest_data, "range_size")
    dev.off()
    
    # Check if the file is created
    expect_true(file.exists(png_file))
    
    # Clean up
    unlink(png_file)
  })
})

test_that("elbow_plot generates plots with different variables", {
  expect_silent({
    # Save plot to a temporary file for each variable
    variables <- c("range_size", "mean_evol_dist", "fun_dist")
    for (variable in variables) {
      png_file <- tempfile(fileext = ".png")
      png(filename = png_file)
      elbow_plot(forest_data, variable)
      dev.off()
      
      # Check if the file is created
      expect_true(file.exists(png_file))
      
      # Clean up
      unlink(png_file)
    }
  })
})

