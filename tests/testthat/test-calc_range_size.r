library(testthat)
library(sf)
library(GDRarity)

test_that("calc_range_size returns expected dataframe with mocked dependencies", {
  test_df <- data.frame(species_name = c("Fake_species1", "Fake_species2"))

  mock_get_range_convex_hulls <- function(data_frame, species_col, species_name, ...) {
    # Return convex hulls only for the requested species
    result <- list()
    for (sp in species_name) {
      if (sp == "Fake_species1") {
        result[[sp]] <- list(
          sf::st_sfc(sf::st_polygon(list(matrix(c(-115.67, 37.03, -115.62, 37.39, -115.67, 37.03), ncol=2, byrow=TRUE))), crs = 4326)
        )
      } else if (sp == "Fake_species2") {
        result[[sp]] <- list(
          sf::st_sfc(sf::st_polygon(list(matrix(c(-115.80, 36.29, -115.53, 36.46, -115.80, 36.29), ncol=2, byrow=TRUE))), crs = 4326)
        )
      }
    }
    return(result)
  }

  mock_get_continent_sf <- function() {
    sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(-180,-90), c(180,-90), c(180,90), c(-180,90), c(-180,-90)
    )))), crs = 4326)
  }

  mock_clip_polygons_to_land <- function(convex_hulls, continent_sf) {
    convex_hulls
  }

  mock_range_sizes <- function(clipped_polygons_list, species_col) {
    species_names <- names(clipped_polygons_list)
    data.frame(
      species_name = species_names,
      range_size = ifelse(species_names == "Fake_species1", 100, 200)
    )
  }

  with_mocked_bindings(
    {
      result <- calc_range_size(test_df, species_col = "species_name", num_cores = 1)
    },
    get_range_convex_hulls = mock_get_range_convex_hulls,
    get_continent_sf = mock_get_continent_sf,
    clip_polygons_to_land = mock_clip_polygons_to_land,
    range_sizes = mock_range_sizes
  )

  # We have 2 species, each call returns polygons only for that species, so total rows = 2
  expect_equal(nrow(result), 2)

  expect_equal(result$species_name, c("Fake_species1", "Fake_species2"))

  expect_equal(result$range_size, c(100, 200))
})
