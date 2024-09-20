library(testthat)
library(sf)

create_mixed_polygons_list <- function() {
  valid_polygon <- st_sfc(st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))))
  
  # Create a valid polygon for testing
  valid_polygon2 <- st_sfc(st_polygon(list(rbind(
    c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)
  ))))
  
  return(list(
    species1 = valid_polygon,
    species2 = valid_polygon2
  ))

}

test_that("range_sizes handles valid polygons", {
  clipped_polygons_list <- create_mixed_polygons_list()
  
  result <- range_sizes(clipped_polygons_list)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$species_name, c("species1", "species2"))
  expect_equal(result$range_size, c(0.000001, 0.000001))
})

test_that("range_sizes handles empty input", {
  result <- range_sizes(NULL)
  
  expect_equal(nrow(result), 0)
  expect_equal(result$species_name, character())
  expect_equal(result$range_size, numeric())
})

test_that("range_sizes handles empty list", {
  result <- range_sizes(list())
  
  expect_equal(nrow(result), 0)
  expect_equal(result$species_name, character())
  expect_equal(result$range_size, numeric())
})

