library(testthat)

# Minimal version of rename_model_code for testing (returns fixed values)
rename_model_code <- function(model) {
  if (model == "GRFRPR") return("EER")
  if (model == "GRGLFRFL") return("Functional Rarity")
  return(model)
}

test_that("map_est_types returns correct rarity types for EER", {
  expect_equal(unname(map_est_types("GRFRPR", "GR-FR+PR+")), "Indicator")
  expect_equal(unname(map_est_types("GRFRPR", "GR+FR+PR+")), "Classically Rare")
  expect_equal(unname(map_est_types("GRFRPR", "GR-FR-PR-")), "Common")
  expect_equal(unname(map_est_types("GRFRPR", "UNKNOWN_LABEL")), "UNKNOWN_LABEL")
})

test_that("map_est_types returns correct rarity types for Functional Rarity", {
  expect_equal(unname(map_est_types("GRGLFRFL", "GR+GL+FR+FL+")), "A")
  expect_equal(unname(map_est_types("GRGLFRFL", "GR-GL-FR-FL-")), "L")
  expect_equal(unname(map_est_types("GRGLFRFL", "IMPOSSIBLE_LABEL")), "IMPOSSIBLE_LABEL")
})

test_that("map_est_types returns label unchanged for other models", {
  expect_equal(unname(map_est_types("OTHER_MODEL", "SOME_LABEL")), "SOME_LABEL")
})

