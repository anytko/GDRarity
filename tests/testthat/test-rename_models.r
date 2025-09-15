library(testthat)

test_that("rename_model_code correctly simplifies RL patterns", {
  codes <- c("GRGL", "FRFL", "PRPL")
  renamed <- rename_model_code(codes)
  
  expect_equal(unname(renamed["GRGL"]), "GRL") # GR and GL → GRRL
  expect_equal(unname(renamed["FRFL"]), "FRL") # FR and FL → FRRL
  expect_equal(unname(renamed["PRPL"]), "PRL") # PR and PL → PRRL
})

test_that("rename_model_code applies alias map correctly", {
  codes <- c("GRGLPR", "GRFRPR", "GRGLFRFL", "GRGLFRFLPRPL")
  renamed <- rename_model_code(codes)
  
  expect_equal(unname(renamed["GRGLPR"]), "EDGE")
  expect_equal(unname(renamed["GRFRPR"]), "EER")
  expect_equal(unname(renamed["GRGLFRFL"]), "Functional Rarity")
  expect_equal(unname(renamed["GRGLFRFLPRPL"]), "GDR")
})

test_that("rename_model_code leaves unrelated codes unchanged", {
  codes <- c("GR", "FL", "PR")
  renamed <- rename_model_code(codes)
  
  expect_equal(unname(renamed["GR"]), "GR")
  expect_equal(unname(renamed["FL"]), "FL")
  expect_equal(unname(renamed["PR"]), "PR")
})

test_that("rename_model_columns renames matching columns correctly", {
  df <- data.frame(GRGL = 1:3, GRFRPR = 4:6, Other = 7:9)
  renamed_df <- rename_model_columns(df, c("GRGL", "GRFRPR"))
  
  expect_true("GRL" %in% colnames(renamed_df)) # From GRGL
  expect_true("EER" %in% colnames(renamed_df))  # Alias for GRFRPR
  expect_true("Other" %in% colnames(renamed_df)) # Unchanged
})

test_that("rename_model_columns ignores non-matching models", {
  df <- data.frame(GRL = 1:3, X = 4:6)
  renamed_df <- rename_model_columns(df, c("GRL", "Nonexistent"))
  
  expect_true("GRL" %in% colnames(renamed_df))
  expect_false("Nonexistent" %in% colnames(renamed_df))
})

