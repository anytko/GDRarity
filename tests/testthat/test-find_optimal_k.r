library(testthat)

test_that("find_optimal_k basic structure works", {
  set.seed(123)
  df <- data.frame(x = rnorm(50))
  
  result <- find_optimal_k(df, variable = "x", k_max = 5)
  
  expect_type(result, "list")
  expect_named(result, c("optimal_k", "slopes", "slope_threshold"), ignore.order = TRUE)
  expect_true(is.numeric(result$optimal_k))
  expect_true(is.data.frame(result$slopes))
  expect_true(is.numeric(result$slope_threshold))
})

test_that("find_optimal_k returns a ggplot object when plot = TRUE", {
  set.seed(123)
  df <- data.frame(x = rnorm(50))
  
  result <- find_optimal_k(df, variable = "x", k_max = 5, plot = TRUE)
  
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("find_optimal_k defaults to k_max if no WCSS below threshold", {
  mock_wcss <- function(data, variable, k_values) {
    rep(100, length(k_values)) # Never drops below threshold
  }
  
  with_mocked_bindings({
    result <- find_optimal_k(data.frame(x = 1:10), "x", k_max = 8)
  }, compute_wcss = mock_wcss)
  
  expect_equal(result$optimal_k, 8)
})

test_that("find_optimal_k reacts to slope_factor changes", {
  df <- data.frame(x = rnorm(50))
  
  res_low <- find_optimal_k(df, "x", k_max = 5, slope_factor = 0.1)
  res_high <- find_optimal_k(df, "x", k_max = 5, slope_factor = 2)
  
  expect_false(identical(res_low$optimal_k, res_high$optimal_k))
})
