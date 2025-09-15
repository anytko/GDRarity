library(testthat)

test_that("gdrare_pipeline runs end-to-end with mock functions", {
  # Mock species input
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1.0, 2.0)
  )

  # Temporarily replace internal functions with mocks
  local_mocked_bindings(
    prepare_gdrarity_models = function(...) {
      list(
        models_to_run = c("GRFR"),
        species_df = species_df,
        abundance_df = NULL,
        phylo = NULL
      )
    },
    prepare_gdrarity_axes = function(models_to_run, species_df, ...) {
      data.frame(
        species = c("sp1", "sp2"),
        GR = c(0.1, 0.2),
        FR = c(0.9, 0.8)
      )
    },
    assign_rarity_types = function(df, models_to_run, ...) {
      df$GRFR <- c("GR+FR+", "GR-FR-")
      return(df)
    }
  )

  result <- gdrare_pipeline(
    species_df = species_df,
    trait_cols = "trait1",
    geo_rarity_method = "taxonomic",
    fun_rarity_method = "mean_distance"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("species", "GR", "FR", "GRFR") %in% names(result)))
  expect_equal(nrow(result), 2)
  expect_equal(result$GRFR, c("GR+FR+", "GR-FR-"))
})


test_that("gdrare_pipeline works end-to-end with a user-specified axis", {
  # Dummy species data with a custom axis
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1.0, 2.0),
    habitat_specificity = c(0.2, 0.8)
  )

  # Mock functions for controlled test
  local_mocked_bindings(
    prepare_gdrarity_models = function(species_df, additional_dimensions, ...) {
      # Ensure the additional axis is passed through
      expect_true("habitat_specificity" %in% names(species_df))
      list(
        models_to_run = c("GRH"),  # Include the custom axis in model
        species_df = species_df,
        abundance_df = NULL,
        phylo = NULL
      )
    },
    prepare_gdrarity_axes = function(models_to_run, species_df, ...) {
      # Pass through GR and habitat_specificity values
      data.frame(
        species = species_df$species,
        GR = c(0.1, 0.3),
        H = species_df$habitat_specificity
      )
    },
    assign_rarity_types = function(df, models_to_run, thresholds, ...) {
      # Verify that custom axis (H) is present
      expect_true("H" %in% colnames(df))
      df$GRH <- c("GR+H+", "GR-H-")
      return(df)
    }
  )

  result <- gdrare_pipeline(
    species_df = species_df,
    trait_cols = "trait1",
    additional_dimensions = list(H = "habitat_specificity"),
    geo_rarity_method = "taxonomic",
    fun_rarity_method = "mean_distance"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("species", "GR", "H", "GRH") %in% names(result)))
  expect_equal(result$GRH, c("GR+H+", "GR-H-"))
})

