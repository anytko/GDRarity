library(testthat)
library(ape) 

test_that("returns a phylo object for ALLMB", {
  tree <- get_phy_angio("ALLMB")
  expect_s3_class(tree, "phylo")
  expect_true(!is.null(tree))
})

test_that("returns a phylo object for ALLOTB", {
  tree <- get_phy_angio("ALLOTB")
  expect_s3_class(tree, "phylo")
  expect_true(!is.null(tree))
})

test_that("returns a phylo object for GBMB", {
  tree <- get_phy_angio("GBMB")
  expect_s3_class(tree, "phylo")
  expect_true(!is.null(tree))
})

test_that("returns a phylo object for GBOTB", {
  tree <- get_phy_angio("GBOTB")
  expect_s3_class(tree, "phylo")
  expect_true(!is.null(tree))
})

test_that("throws an error for invalid input", {
  expect_error(
    get_phy_angio("INVALID"), 
    "Invalid choice. Please specify 'ALLMB', 'ALLOTB', 'GBMB', or 'GBOTB'."
  )
})
