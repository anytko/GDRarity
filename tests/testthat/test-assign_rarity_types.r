library(testthat)
library(GDRarity)

test_that("assign_rarity_types applies percentile thresholds correctly", {
  df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    GR = c(0.05, 0.2, 0.8),
    FR = c(0.95, 0.2, 0.5)
  )
  
  result <- assign_rarity_types(
    df = df,
    models_to_run = "GRFR",
    k_means = FALSE,
    thresholds = list(GR = 0.15, FR = 0.75)
  )
  
  # Check raw columns exist
  expect_true(all(c("GR_raw", "FR_raw") %in% colnames(result)))
  
  # GR low threshold → sp1 rare
  expect_equal(result$GR_flag, c("+", "-", "-"))
  
  # FR high threshold → sp1 rare
  expect_equal(result$FR_flag, c("+", "-", "-"))
})

test_that("assign_rarity_types uses k-means thresholds when requested", {
  set.seed(123)
  df <- data.frame(
    species = paste0("sp", 1:10),
    GR = c(runif(5, 0, 0.2), runif(5, 0.8, 1))
  )
  
  result <- assign_rarity_types(
    df = df,
    models_to_run = "GR",
    k_means = TRUE,
    thresholds = list(GR = 0.15)
  )
  
  expect_true(all(result$GR_flag %in% c("+", "-")))
  # Check that both + and - are present
  expect_true(length(unique(result$GR_flag)) == 2)
})

test_that("assign_rarity_types falls back to percentile if too few data points for k-means", {
  df <- data.frame(
    species = paste0("sp", 1:3),
    GR = c(0.1, 0.2, 0.3)
  )
  
  expect_warning(
    result <- assign_rarity_types(
      df = df,
      models_to_run = "GR",
      k_means = TRUE,
      thresholds = list(GR = 0.15)
    ),
    "Too few data points for k-means"
  )
  
  expect_true("GR_flag" %in% colnames(result))
})

test_that("assign_rarity_types respects custom directions", {
  df <- data.frame(
    species = c("sp1", "sp2"),
    GR = c(0.9, 0.1)
  )
  
  result <- assign_rarity_types(
    df = df,
    models_to_run = "GR",
    k_means = FALSE,
    thresholds = list(GR = 0.15),
    directions = list(GR = "high")
  )
  
  # With high direction, sp1 rare
  expect_equal(result$GR_flag, c("+", "-"))
})

test_that("assign_rarity_types handles special model names EER and Functional Rarity", {
  df <- data.frame(
    species = c("sp1", "sp2"),
    GR = c(0.1, 0.9),
    FR = c(0.95, 0.2),
    PR = c(0.92, 0.3),
    GL = c(0.1, 0.9),
    FL = c(0.95, 0.2)
  )
  
  result <- assign_rarity_types(
    df = df,
    models_to_run = c("EER", "Functional Rarity"),
    k_means = FALSE,
    thresholds = list(GR = 0.15, FR = 0.75, PR = 0.9, GL = 0.15, FL = 0.9)
  )
  
  expect_true("EER" %in% colnames(result))
  expect_true("Functional Rarity" %in% colnames(result))
})

test_that("assign_rarity_types warns and marks missing axes with '?'", {
  df <- data.frame(
    species = c("sp1", "sp2"),
    GR = c(0.1, 0.2) # FR is missing
  )

  expect_warning(
    result <- assign_rarity_types(
      df = df,
      models_to_run = "GRFR",
      k_means = FALSE,
      thresholds = list(GR = 0.15, FR = 0.75)
    ),
    regexp = "missing from the input data frame"
  )

  # Missing FR axis should produce '?'
  expect_true(all(grepl("\\?", result$GRFR)))
  expect_equal(result$GR_flag, c("+", "-"))
})
