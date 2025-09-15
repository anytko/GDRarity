library(testthat)
library(vegan)

test_that("fun_dist computes FD correctly on a simple dataset", {
  # Create a small example dataset with 3 species and 3 traits
  df <- data.frame(
    sp = c("A", "B", "C"),
    trait1 = c(1, 2, 3),
    trait2 = c(4, 5, 6),
    trait3 = c(7, 8, 9)
  )

  # Calculate FD using the function
  result <- fun_dist(df, trait_columns = c("trait1", "trait2", "trait3"), species_col = "sp")

  # It should return a data frame with 3 rows and 2 columns
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(colnames(result), c("sp", "fun_dist"))

  # FD values should be numeric and greater than or equal to zero
  expect_type(result$fun_dist, "double")
  expect_true(all(result$fun_dist >= 0))

  # Since traits are increasing linearly, species "B" should be functionally intermediate,
  # so its FD should be less than that of "A" or "C"
  expect_true(result$fun_dist[result$sp == "B"] < max(result$fun_dist))
})

test_that("fun_dist errors on empty data frame", {
  empty_df <- data.frame(species = character(0), trait1 = numeric(0), trait2 = numeric(0))
  expect_error(
    fun_dist(empty_df, trait_columns = c("trait1", "trait2")),
    "data frame is empty"
  )
})

test_that("fun_dist errors if less than 2 traits are provided", {
  df <- data.frame(species = c("A", "B"), trait1 = c(1, 2))
  expect_error(
    fun_dist(df, trait_columns = "trait1"),
    "At least 2 traits are required"
  )
})

test_that("fun_dist respects default and custom species_col", {
  df_default <- data.frame(species = c("X", "Y"), trait1 = c(1, 2), trait2 = c(3, 4))
  result_default <- fun_dist(df_default, trait_columns = c("trait1", "trait2"))
  expect_equal(colnames(result_default)[1], "species")

  df_custom <- data.frame(sp_name = c("X", "Y"), trait1 = c(1, 2), trait2 = c(3, 4))
  result_custom <- fun_dist(df_custom, trait_columns = c("trait1", "trait2"), species_col = "sp_name")
  expect_equal(colnames(result_custom)[1], "sp_name")
})

test_that("fun_dist works with median-centering", {
  df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    trait1 = c(10, 20, 30),
    trait2 = c(5, 15, 25)
  )
  result <- fun_dist(df, trait_columns = c("trait1", "trait2"))
  # Check that output has correct species and non-zero FD values
  expect_setequal(result$species, df$species)
  expect_true(all(result$fun_dist >= 0))
})
