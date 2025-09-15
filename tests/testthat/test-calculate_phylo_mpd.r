library(testthat)
library(ape)

test_that("calculate_phylo_mpd works with weighted and relative", {
  phy <- ape::rtree(4)
  phy$tip.label <- c("Species1", "Species2", "Species3", "Species4")

  df <- data.frame(
    species = c("Species1", "Species2", "Species3", "Species4"),
    site = c("SiteA", "SiteA", "SiteA", "SiteA"),
    presence_absence = 1,
    abundance = c(10, 5, 3, 8)
  )

  result <- calculate_phylo_mpd(phy, df, relative = TRUE, weighted = TRUE)

  expect_true("EU" %in% names(result))
  expect_equal(nrow(result), nrow(df))
  expect_true(all(result$EU >= 0 & result$EU <= 1, na.rm = TRUE))
})

test_that("calculate_phylo_mpd works without weighting", {
  set.seed(42)
  phy <- ape::rtree(3)
  phy$tip.label <- c("A", "B", "C")

  df <- data.frame(
    species = c("A", "B", "C"),
    site = c("X", "X", "X"),
    presence_absence = 1
  )

  result <- calculate_phylo_mpd(phy, df, relative = FALSE, weighted = FALSE)

  expect_true(all(!is.na(result$EU)))
  expect_gt(max(result$EU, na.rm = TRUE), 1) # Because not scaled
})

test_that("returns NA EU when only one species in a site", {
  phy <- ape::rtree(2)
  phy$tip.label <- c("S1", "S2")

  df <- data.frame(
    species = c("S1"),
    site = c("Solo"),
    presence_absence = 1,
    abundance = 5
  )

  result <- calculate_phylo_mpd(phy, df)

  expect_true(is.na(result$EU[1]))
})

test_that("errors if abundance missing when weighted = TRUE", {
  phy <- ape::rtree(3)
  phy$tip.label <- LETTERS[1:3]

  df <- data.frame(
    species = c("A", "B", "C"),
    site = c("Site1", "Site1", "Site1"),
    presence_absence = 1
  )

  expect_error(calculate_phylo_mpd(phy, df, weighted = TRUE), "must include")
})

test_that("relative scaling normalizes max EU to 1", {
  phy <- ape::rtree(4)
  phy$tip.label <- paste0("Sp", 1:4)

  df <- data.frame(
    species = paste0("Sp", 1:4),
    site = rep("Site1", 4),
    presence_absence = 1,
    abundance = 1
  )

  result <- calculate_phylo_mpd(phy, df, relative = TRUE, weighted = FALSE)
  expect_lte(max(result$EU, na.rm = TRUE), 1)
})
