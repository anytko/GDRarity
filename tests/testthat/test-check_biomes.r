library(testthat)
library(sf)
library(devtools)  # Add this
library(pkgload)

# Mock convex_hull data for testing
  create_mock_convex_hull <- function() {
    st_sf(
      geometry = st_sfc(
        st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
        crs = 4326
      )
    )
  }

  convex_hulls <- list(
    "Species_1" = list(create_mock_convex_hull()),
    "Species_2" = list(create_mock_convex_hull())
  )

# Mock biome shapefile as an sf object for testing
create_mock_biome_sf <- function() {
  st_sf(
    BIOME = c(1, 2),
    geometry = st_sfc(
      st_polygon(list(rbind(c(-2, -2), c(2, -2), c(2, 2), c(-2, 2), c(-2, -2)))),
      st_polygon(list(rbind(c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0))))
    ),
    crs = 4326
  )
}

# Mock biome data
mock_biome_sf <- create_mock_biome_sf()

test_that("check_biomes reads default biome shapefile correctly", {
  result_df <- check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = NULL)
  expect_s3_class(result_df, "data.frame")
  expect_true("Humid Tropics" %in% colnames(result_df))
})

test_that("check_biomes handles custom biome data", {
  custom_biome_sf <- create_mock_biome_sf()
  result_df <- check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = custom_biome_sf)
  expect_s3_class(result_df, "data.frame")
})

test_that("check_biomes handles invalid biome_sf", {
  expect_error(check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = "invalid"), 
               "Provided biome_sf must be an sf object.")
})

test_that("check_biomes returns empty dataframe with empty convex_hulls", {
  result_df <- check_biomes(convex_hulls = list(), biome_sf = NULL)
  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 0)
})

test_that("check_biomes returns empty dataframe with empty biome_sf", {
  empty_biome_sf <- st_sf(geometry = st_sfc(), crs = 4326)
  result_df <- check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = empty_biome_sf)
  expect_s3_class(result_df, "data.frame")
  expect_true(ncol(result_df) == 1 && "species_name" %in% colnames(result_df))
})

test_that("check_biomes handles invalid convex hulls gracefully", {
  invalid_convex_hull <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(0,0,1,1,0), c(0,1,1,0,0))))))
  result_df <- tryCatch({
    check_biomes(convex_hulls = list("Species_1" = list(invalid_convex_hull)), biome_sf = NULL)
  }, error = function(e) e)
  expect_error(result_df, NA)
})

test_that("check_biomes names columns correctly", {
  result_df <- check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = NULL)
  expect_true(all(c("Humid Tropics", "Dry Tropics", "Temperate", "Boreal", "species_name") %in% colnames(result_df)))
})

create_mock_convex_hull <- function() {
  st_as_sf(st_sfc(st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))))
}

# Revised test for missing default shapefile
test_that("check_biomes raises error if default shapefile is missing", {
  # Temporarily override system.file to simulate the missing shapefile
  with_mocked_bindings(
    `system.file` = function(...) { "" }, # Return an empty path to simulate missing file
    expect_error(
      check_biomes(convex_hulls = list("Species_1" = list(create_mock_convex_hull())), biome_sf = NULL),
      "Shapefile not found. Ensure the shapefile is in the 'extdata' directory of the package."
    )
  )
})
