library(testthat)
library(ape)

test_that("prepare_gdrarity_models works with minimal data (species only)", {
  species_df <- data.frame(species = c("sp1", "sp2", "sp3"))
  
  # Should error because less than 2 axes available (only GR)
  expect_warning(
    prepare_gdrarity_models(species_df),
    "No species matched internal phylogeny. Phylogenetic rarity will not be calculated."
  )
})

test_that("prepare_gdrarity_models recognizes abundance and traits correctly", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1.1, 2.2)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(5, 10)
  )
  
  result <- prepare_gdrarity_models(
    species_df = species_df,
    abundance_df = abundance_df,
    trait_cols = "trait1",
    use_internal_phylo = FALSE
  )
  
  # Should have these axes available
  expect_true(all(c("GR", "GL", "FR", "FL") %in% result$available_axes))
  expect_null(result$phylo)  # No phylo provided, and internal disabled
  
  # models_to_run should include combinations of these axes
  expect_true(any(grepl("GRGLFRFL", result$models_to_run)))
})

test_that("prepare_gdrarity_models handles provided phylo and prunes species", {
  phylo <- rtree(4)
  phylo$tip.label <- c("sp1", "sp2", "sp3", "spX")
  species_df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    trait1 = c(1, 2, 3)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    site = c("A", "A", "A"),
    abundance = c(1, 2, 3)
  )
  
  result <- prepare_gdrarity_models(
    species_df = species_df,
    abundance_df = abundance_df,
    trait_cols = "trait1",
    phylo = phylo,
    use_internal_phylo = FALSE
  )
  
  expect_true("PR" %in% result$available_axes)
  expect_true("PL" %in% result$available_axes)
  expect_true(inherits(result$phylo, "phylo"))
  expect_false("spX" %in% result$phylo$tip.label)
})

test_that("prepare_gdrarity_models returns most complete model when requested", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(3, 5)
  )
  
  result <- prepare_gdrarity_models(
    species_df = species_df,
    abundance_df = abundance_df,
    trait_cols = "trait1",
    use_internal_phylo = FALSE,
    use_most_complete_model = TRUE
  )
  
  # Only one model returned: the full combination sorted
  expect_length(result$models_to_run, 1)
  expect_true(grepl("GR", result$models_to_run))
  expect_true(grepl("GL", result$models_to_run))
  expect_true(grepl("FR", result$models_to_run))
  expect_true(grepl("FL", result$models_to_run))
})

test_that("prepare_gdrarity_models warns if species do not match phylo", {
  phylo <- rtree(3)
  phylo$tip.label <- c("spX", "spY", "spZ")
  species_df <- data.frame(species = c("sp1", "sp2"))
  
  expect_warning(
    prepare_gdrarity_models(
      species_df = species_df,
      phylo = phylo,
      use_internal_phylo = FALSE
    ),
    "None of the species match the provided phylogeny. Phylogenetic rarity will not be calculated."
  )
})

test_that("prepare_gdrarity_models errors if species_col not found", {
  species_df <- data.frame(not_species = c("sp1"))
  
  expect_error(
    prepare_gdrarity_models(species_df),
    "Species column not found"
  )
})

test_that("prepare_gdrarity_models errors if specified model is unavailable", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(3, 5)
  )
  
  expect_error(
    prepare_gdrarity_models(
      species_df = species_df,
      abundance_df = abundance_df,
      trait_cols = "trait1",
      use_internal_phylo = FALSE,
      model = c("NON_EXISTENT_MODEL")
    ),
    "None of the specified models"
  )
})

test_that("prepare_gdrarity_models warns when some requested models are missing", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(3, 5)
  )
  
  expect_warning(
    prepare_gdrarity_models(
      species_df = species_df,
      abundance_df = abundance_df,
      trait_cols = "trait1",
      use_internal_phylo = FALSE,
      model = c("GRFR", "NON_EXISTENT_MODEL")
    ),
    "Some specified models were not available"
  )
})

test_that("prepare_gdrarity_models handles additional_dimensions properly", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2),
    HabitatType = c("forest", "grassland")
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(3, 5)
  )
  
  result <- prepare_gdrarity_models(
    species_df = species_df,
    abundance_df = abundance_df,
    trait_cols = "trait1",
    use_internal_phylo = FALSE,
    additional_dimensions = "HabitatType"
  )
  
  # The additional dimension code should be present in models_to_run
  expect_true(any(grepl("^H", result$available_axes)))
  expect_true(any(grepl("^H", result$models_to_run)))
})

test_that("prepare_gdrarity_models errors if additional_dimensions missing in species_df", {
  species_df <- data.frame(
    species = c("sp1", "sp2"),
    trait1 = c(1, 2)
  )
  abundance_df <- data.frame(
    species = c("sp1", "sp2"),
    site = c("A", "A"),
    abundance = c(3, 5)
  )
  
  expect_error(
    prepare_gdrarity_models(
      species_df = species_df,
      abundance_df = abundance_df,
      trait_cols = "trait1",
      use_internal_phylo = FALSE,
      additional_dimensions = "NotAColumn"
    ),
    "One or more additional_dimensions not found"
  )
})
