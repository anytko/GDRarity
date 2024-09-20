test_that("scale_by_median scales columns correctly with default columns", {
  df <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(10, 20, 30, 40),
    trait2 = c(100, 200, 300, 400)
  )
  
  result <- scale_by_median(df)
  
  expected_result <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(-1.5, -0.5, 0.5, 1.5),
    trait2 = c(-1.5, -0.5, 0.5, 1.5)
  )
  
  expect_equal(result, expected_result)
})

test_that("scale_by_median handles NA values correctly", {
  df <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(10, NA, 30, 40),
    trait2 = c(NA, 200, 300, NA)
  )
  
  expect_error(
    scale_by_median(df),
    "Dataframe contains NA values. Please remove or impute NA values before applying the function."
  )
})

test_that("scale_by_median scales only specified columns", {
  df <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(10, 20, 30, 40),
    trait2 = c(100, 200, 300, 400)
  )
  
  result <- scale_by_median(df, columns_chosen = "trait1")
  
  expected_result <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(-1.5, -0.5, 0.5, 1.5),
    trait2 = c(100, 200, 300, 400)  # Unchanged
  )
  
  expect_equal(result, expected_result)
})

test_that("scale_by_median works with an empty data frame", {
  df <- data.frame(species_name = character(), trait1 = numeric())
  
  result <- scale_by_median(df)
  
  expect_equal(result, df)
})

test_that("scale_by_median handles columns with a single unique value", {
  df <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(10, 10, 10, 10),
    trait2 = c(100, 200, 300, 400)
  )
  
  result <- scale_by_median(df)
  
  expected_result <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(NaN, NaN, NaN, NaN),  # Single value, MAD = 0
    trait2 = c(-1.5, -0.5, 0.5, 1.5)
  )
  
  expect_equal(result, expected_result)
})

test_that("scale_by_median preserves column names", {
  df <- data.frame(
    species_name = c("A", "B", "C", "D"),
    trait1 = c(10, 20, 30, 40)
  )
  
  result <- scale_by_median(df)
  
  expect_equal(names(result), c("species_name", "trait1"))
})