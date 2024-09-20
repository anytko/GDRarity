library(testthat)
library(ape) 

test_that("get_phy_angio works correctly", {
  
  # Test 1: Valid input 'ALLMB' - ensure a phylo object is returned
  test_that("returns a phylo object for ALLMB", {
    tree <- get_phy_angio("ALLMB")
    expect_s3_class(tree, "phylo") # Check that the output is of class 'phylo'
    expect_true(!is.null(tree))    # Ensure tree is not null
  })
  
  # Test 2: Valid input 'ALLOTB' - ensure a phylo object is returned
  test_that("returns a phylo object for ALLOTB", {
    tree <- get_phy_angio("ALLOTB")
    expect_s3_class(tree, "phylo")
    expect_true(!is.null(tree))
  })
  
  # Test 3: Valid input 'GBMB' - ensure a phylo object is returned
  test_that("returns a phylo object for GBMB", {
    tree <- get_phy_angio("GBMB")
    expect_s3_class(tree, "phylo")
    expect_true(!is.null(tree))
  })
  
  # Test 4: Valid input 'GBOTB' - ensure a phylo object is returned
  test_that("returns a phylo object for GBOTB", {
    tree <- get_phy_angio("GBOTB")
    expect_s3_class(tree, "phylo")
    expect_true(!is.null(tree))
  })
  
  # Test 5: Invalid input - ensure function throws an error
  test_that("throws an error for invalid input", {
    expect_error(get_phy_angio("INVALID"), 
                 "Invalid choice. Please specify 'ALLMB', 'ALLOTB', 'GBMB', or 'GBOTB'.")
  })

})

