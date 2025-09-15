library(testthat)
library(dplyr)

test_that("compute_mean_si returns correct structure and values", {
  skip_if_not_installed("funrar")
  
  df <- data.frame(
    species = c("sp1", "sp2", "sp1", "sp3", "sp2", "sp3"),
    site = c("A", "A", "B", "B", "C", "C"),
    abundance = c(10, 5, 3, 8, 2, 7)
  )
  
  result <- compute_mean_si(df)
  
  # Check result is a tibble or data frame
  expect_s3_class(result, "data.frame")
  
  # Check it has the right columns
  expect_true(all(c("species", "Si") %in% colnames(result)))
  
  # Check Si values are numeric and within [0,1]
  expect_type(result$Si, "double")
  expect_true(all(result$Si >= 0 & result$Si <= 1))
  
  # Check that species from input appear in output
  expect_setequal(result$species, unique(df$species))
})

test_that("compute_mean_si works with custom column names", {
  skip_if_not_installed("funrar")
  
  df <- data.frame(
    sp = c("sp1", "sp2", "sp1", "sp3", "sp2", "sp3"),
    site_id = c("A", "A", "B", "B", "C", "C"),
    ab = c(10, 5, 3, 8, 2, 7)
  )
  
  result <- compute_mean_si(df, species_col = "sp", site_col = "site_id", abundance_col = "ab")
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("sp", "Si") %in% colnames(result)))
})


test_that("compute_mean_si errors when all abundances are zero", {
  df_zero_abund <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(0, 0)
  )

  expect_error(
    compute_mean_si(df_zero_abund),
    "Scarcity cannot be computed because all abundance values are zero."
  )
})
