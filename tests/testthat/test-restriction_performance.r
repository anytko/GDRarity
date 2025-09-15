library(testthat)
library(mockery)
library(ape)   
library(dplyr)


mock_gdrare_pipeline <- function(...) {
  args <- list(...)
  species_df <- args$species_df
  n <- nrow(species_df)
  species_vec <- species_df[[args$species_col]]
  
  flags <- rep(c("+", "-"), length.out = n)
  
  # Make a dummy dataframe with flags and some numeric columns
  data.frame(
    species = species_vec,
    GR_flag = flags,
    GL_flag = rev(flags),
    FR_flag = flags,
    PR_flag = rev(flags),
    GR = runif(n, 0.1, 0.2),
    GL = runif(n, 0.1, 0.2),
    FR = runif(n, 0.9, 1.0),
    PR = runif(n, 0.7, 0.8),
    range_diff = species_df$range_diff,
    flowering_duration = species_df$flowering_duration,
    stringsAsFactors = FALSE
  )
}

# --- Test run_all_gdrare_combos ---

test_that("run_all_gdrare_combos outputs correct structure and columns", {
  species_vec <- paste0("sp", 1:10)
  set.seed(123)
  
  species_df <- data.frame(
    species = species_vec,
    range_diff = sample(10:30, 10, replace = TRUE),
    flowering_duration = sample(3:10, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  abundance_df <- data.frame(
    species = rep(species_vec, each = 2),
    site = rep(c("A", "B"), times = 10),
    presence_absence = sample(0:1, 20, replace = TRUE),
    abundance = sample(1:20, 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  tree <- rtree(n = length(species_vec), tip.label = species_vec)
  tree_ultra <- chronos(tree)
  class(tree_ultra) <- c("phylo", "chronos")
  
  stub(run_all_gdrare_combos, "gdrare_pipeline", mock_gdrare_pipeline)
  
  combos <- run_all_gdrare_combos(
    species_df = species_df,
    abundance_df = abundance_df,
    geo_methods = c("taxonomic"),
    threshold_sets = list(list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.90, PL = 0.90)),
    direction_sets = list(list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high")),
    use_internal_phylo = FALSE,
    phylo = tree_ultra
  )
  
  expect_type(combos, "list")
  
  # Check keys
  expect_true(all(c("results", "threshold_sets", "direction_sets", "param_grid") %in% names(combos)))
  
  # Results should be a list of data.frames
  expect_true(is.list(combos$results))
  expect_true(all(sapply(combos$results, is.data.frame)))
  
  # Check that the species column exists in first result
  expect_true("species" %in% colnames(combos$results[[1]]))
  
  # Check some flag columns and their expected values
  expect_true("GR_flag" %in% colnames(combos$results[[1]]))
  expect_true(all(combos$results[[1]]$GR_flag %in% c("+", "-")))
  
  # threshold_sets and direction_sets are data frames with rows matching length of input lists
  expect_true(is.data.frame(combos$threshold_sets))
  expect_equal(nrow(combos$threshold_sets), 1)
  expect_true(is.data.frame(combos$direction_sets))
  expect_equal(nrow(combos$direction_sets), 1)
  
  # param_grid is a data.frame with expected columns
  expect_true(is.data.frame(combos$param_grid))
  expect_true(all(c("geo_rarity_method", "fun_rarity_method", "abundance", "threshold_set") %in% colnames(combos$param_grid)))
})

# --- Test eval_restriction_performance ---

test_that("eval_restriction_performance runs and outputs correct columns", {
  # Prepare minimal df_list input: one data frame with numeric restrictions columns
  df <- data.frame(
    species = paste0("sp", 1:10),
    GR_flag = rep(c("+", "-"), 5),
    GL_flag = rep(c("-", "+"), 5),
    restriction1 = runif(10),
    restriction2 = runif(10),
    range_diff = runif(10, 10, 30),
    flowering_duration = runif(10, 3, 10)
  )
  
  df_list <- list(df)
  response_vars <- c("range_diff", "flowering_duration")
  
  res <- eval_restriction_performance(df_list, response_vars)
  
  expect_true(is.data.frame(res))
  expect_true(all(c("method", "model", "response_variable", "AIC", "df", "F_statistic", "p_value", "multiple_R2", "adjusted_R2") %in% colnames(res)))
  
  # Should have rows for each restriction and each response var
  expect_true(nrow(res) >= length(response_vars)) 
})

# --- Test get_best_methods ---

test_that("get_best_methods filters and sorts ANOVA results correctly", {
  anova_df <- data.frame(
    response_variable = rep(c("range_diff", "flowering_duration"), each = 3),
    model = rep(c("res1", "res2", "res3"), 2),
    AIC = c(100, 95, 98, 110, 105, 102),
    stringsAsFactors = FALSE
  )
  
  best <- get_best_methods(anova_df)
  
  # Check it's a data.frame/tibble and columns exist
  expect_true(is.data.frame(best))
  expect_true(all(c("response_variable", "model", "AIC") %in% colnames(best)))
  
  # Check number of rows: one per unique (response_variable, model)
  expected_n <- length(unique(paste(anova_df$response_variable, anova_df$model)))
  expect_equal(nrow(best), expected_n)
  
  # For each (response_variable, model) combo, AIC is minimal in the group
  for (rv_mod in unique(paste(anova_df$response_variable, anova_df$model))) {
    rv <- sub(" .*", "", rv_mod)
    mod <- sub(".* ", "", rv_mod)
    subset_rows <- anova_df[anova_df$response_variable == rv & anova_df$model == mod, ]
    best_row <- best[best$response_variable == rv & best$model == mod, ]
    
    expect_equal(best_row$AIC, min(subset_rows$AIC))
  }
  
  # Check rows sorted by response_variable then AIC ascending
  best_sorted <- best[order(best$response_variable, best$AIC), ]
  expect_equal(best, best_sorted)
})

# --- Test restriction_performance_pipeline ---

test_that("restriction_performance_pipeline runs and returns all expected outputs", {
  species_vec <- paste0("sp", 1:10)
  set.seed(123)
  
  species_df <- data.frame(
    species = species_vec,
    range_diff = sample(10:30, 10, replace = TRUE),
    flowering_duration = sample(3:10, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  abundance_df <- data.frame(
    species = rep(species_vec, each = 2),
    site = rep(c("A", "B"), times = 10),
    presence_absence = sample(0:1, 20, replace = TRUE),
    abundance = sample(1:20, 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  tree <- rtree(n = length(species_vec), tip.label = species_vec)
  tree_ultra <- chronos(tree)
  class(tree_ultra) <- c("phylo", "chronos")
  
  # Stub gdrare_pipeline inside run_all_gdrare_combos
  stub(run_all_gdrare_combos, "gdrare_pipeline", mock_gdrare_pipeline)
  
  res <- restriction_performance_pipeline(
    species_df = species_df,
    abundance_df = abundance_df,
    geo_methods = c("taxonomic"),
    threshold_sets = list(list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.90, PL = 0.90)),
    direction_sets = list(list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high")),
    use_internal_phylo = FALSE,
    phylo = tree_ultra,
    response_vars = c("range_diff", "flowering_duration")
  )
  
  expect_type(res, "list")
  expect_true(all(c("combos", "param_grid", "threshold_sets", "direction_sets", "anova_results", "best_models") %in% names(res)))
  
  # combos is list with expected keys
  expect_true(is.list(res$combos))
  expect_true(all(c("results", "threshold_sets", "direction_sets", "param_grid") %in% names(res$combos)))
  
  # anova_results and best_models are data frames with expected columns
  expect_true(is.data.frame(res$anova_results))
  expect_true(is.data.frame(res$best_models))
  expect_true(all(c("model", "response_variable", "AIC", "p_value") %in% colnames(res$anova_results)))
})

