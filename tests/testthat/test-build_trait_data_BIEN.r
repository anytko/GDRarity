library(testthat)
library(dplyr)

# Now you can write tests for both functions

test_that("build_trait_data_BIEN works with mocked BIEN calls", {
  # Define the mock inside here or outside
  
  mock_BIEN_trait_traitbyspecies <- function(species, trait) {
    tibble(
      scrubbed_species_binomial = rep("Fake species", 2),
      trait_name = rep(trait, 2),
      trait_value = c(10, 15),
      unit = c("unit", "unit"),
      method = c("method", "method"),
      url_source = c("source", "source"),
      project_pi = c("Dr. A", "Dr. A"),
      project_pi_contacts = c("a@example.com", "a@example.com")
    )
  }

  unlink(file.path(tempdir(), "BIEN_cache"), recursive = TRUE)

  result <- with_mocked_bindings(
    {
      result <- build_trait_data_BIEN(
        species = "Fake species",
        traits = c("leaf area", "seed mass")
      )
    },
    BIEN_trait_traitbyspecies = mock_BIEN_trait_traitbyspecies
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("species_name", "leaf area", "seed mass") %in% names(result)))
})

test_that("collapse_BIEN_traits aggregates author info and traits correctly", {
  df <- tibble(
    species_name = c("sp1", "sp1", "sp2", "sp2"),
    project_pi = c("A", "A", "B", NA),
    project_pi_contacts = c("a@example.com", "a@example.com", "b@example.com", NA),
    leaf_area = c(10, 12, 15, NA),
    seed_mass = c(100, 110, NA, 120),
    flower_color = c("red", "red", "blue", "blue")
  )

  collapsed <- collapse_BIEN_traits(df, species_col = "species_name")

  expect_equal(nrow(collapsed), 2)
  expect_equal(collapsed$species_name, c("sp1", "sp2"))
  expect_equal(collapsed$project_pi, c("A", "B"))
  expect_equal(collapsed$project_pi_contacts, c("a@example.com", "b@example.com"))
  expect_equal(collapsed$leaf_area, c(mean(c(10,12)), 15))
  expect_equal(collapsed$seed_mass, c(mean(c(100,110)), mean(c(NA,120), na.rm=TRUE)))
  expect_equal(collapsed$flower_color, c("red", "blue"))
})
