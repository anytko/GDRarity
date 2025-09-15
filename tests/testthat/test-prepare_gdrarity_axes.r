library(testthat)

test_that("prepare_gdrarity_axes computes GR from precomputed values", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    GR = c(0.2, 0.8)
  )

  result <- prepare_gdrarity_axes(
    models_to_run = "GR",
    species_df = species_df,
    geo_rarity_method = "taxonomic",
    use_precomputed_axes = TRUE,   # NEW
    abundance_df = data.frame(     # Needed for taxonomic method with GL fallback
      species = c("sp1", "sp2"),
      site = c("A", "B"),
      abundance = c(1, 1)
    )
  )

  expect_equal(result$GR, c(0.2, 0.8))
})

test_that("prepare_gdrarity_axes errors if FL requested without abundance_df", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2),
    trait2 = c(3, 4)
  )

  expect_error(
    prepare_gdrarity_axes(
      models_to_run = "FL",
      species_df = species_df,
      trait_columns = c("trait1", "trait2")
    ),
    "FL requires abundance_df"
  )
})

test_that("prepare_gdrarity_axes errors if PL requested without phylogeny", {
  species_df <- data.frame(species = c("sp1", "sp2"))

  expect_error(
    prepare_gdrarity_axes(
      models_to_run = "PL",
      species_df = species_df,
      abundance_df = data.frame(
        species = c("sp1", "sp2"),
        site = c("A", "B"),
        abundance = c(1, 2)
      )
    ),
    "PL requires phylogeny"
  )
})

test_that("prepare_gdrarity_axes computes custom axis H", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    H = c(0.3, 0.7)
  )

  result <- prepare_gdrarity_axes(
    models_to_run = "H",
    species_df = species_df,
    use_precomputed_axes = TRUE
  )

  expect_equal(result$H, c(0.3, 0.7))
})

test_that("prepare_gdrarity_axes returns multiple axes when available", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    GR = c(0.2, 0.8),
    FR = c(0.4, 0.6)
  )

  result <- prepare_gdrarity_axes(
    models_to_run = c("GR", "FR"),
    species_df = species_df,
    use_precomputed_axes = TRUE
  )

  expect_true(all(c("GR", "FR") %in% colnames(result)))
})

test_that("prepare_gdrarity_axes errors when no axes can be computed", {
  species_df <- data.frame(species = c("sp1", "sp2"))

  expect_error(
    prepare_gdrarity_axes(
      models_to_run = "FR",
      species_df = species_df,
      trait_columns = NULL
    ),
    "FR requires abundance_df"
  )
})
