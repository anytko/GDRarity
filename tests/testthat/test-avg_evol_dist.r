library(testthat)
library(ape)
library(picante)

test_that("avg_evol_dist works without time slicing", {
  set.seed(1)
  phy <- rtree(5)
  phy$tip.label <- paste0("sp", 1:5)
  df <- data.frame(species = phy$tip.label)

  res <- avg_evol_dist(phy, df, species_col = "species", time = FALSE, num_cores = 1)
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("species", "ED") %in% names(res)))
  expect_equal(nrow(res), 5)
  expect_true(all(!is.na(res$ED)))
})

test_that("avg_evol_dist works with time slicing - non-ultrametric tree warns", {
  set.seed(1)
  phy <- rtree(5)
  phy$tip.label <- paste0("sp", 1:5)
  df <- data.frame(species = phy$tip.label)

  root_age <- max(branching.times(phy))
  slices <- c(0, seq(0.1, root_age - 0.1, length.out = 2))

  expect_warning(
    res <- avg_evol_dist(phy, df, species_col = "species", time = TRUE, 
                         time_slices = slices, num_cores = 1),
    "not ultrametric"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
})

test_that("avg_evol_dist works with time slicing - ultrametric tree no warning", {
  set.seed(1)
  phy <- rtree(5)
  phy$tip.label <- paste0("sp", 1:5)
  phy_ultra <- ape::chronos(phy, lambda = 1, control = chronos.control())
  class(phy_ultra) <- "phylo"
  df <- data.frame(species = phy_ultra$tip.label)

  root_age <- max(branching.times(phy_ultra))
  slices <- c(seq(0.1, root_age - 0.1, length.out = 3))

  expect_silent(
    res <- avg_evol_dist(phy_ultra, df, species_col = "species", time = TRUE, 
                         time_slices = slices, num_cores = 1)
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_true(all(!is.na(res$ED)))
})


test_that("avg_evol_dist filters to selected taxa", {
  set.seed(1)
  phy <- rtree(5)
  phy$tip.label <- paste0("sp", 1:5)
  df <- data.frame(species = phy$tip.label)

  res <- avg_evol_dist(phy, df, species_col = "species", taxon = "sp1", time = FALSE, num_cores = 1)
  expect_equal(nrow(res), 1)
  expect_equal(res$species, "sp1")
})

test_that("avg_evol_dist errors on empty dataframe", {
  phy <- rtree(3)
  df <- data.frame(species = character(0))
  expect_error(avg_evol_dist(phy, df), "data frame is empty")
})

test_that("avg_evol_dist errors on non-phylo input", {
  df <- data.frame(species = "sp1")
  expect_error(avg_evol_dist("not_a_tree", df), "not of class 'phylo'")
})

test_that("avg_evol_dist errors when time slice exceeds root age", {
  phy <- rtree(3)
  df <- data.frame(species = phy$tip.label)
  root_age <- max(branching.times(phy))
  expect_error(
    avg_evol_dist(phy, df, time_slices = root_age + 1),
    "exceed the root age"
  )
})

test_that("avg_evol_dist errors when num_cores < 1", {
  phy <- rtree(3)
  df <- data.frame(species = phy$tip.label)
  expect_error(avg_evol_dist(phy, df, num_cores = 0), "at least 1")
})