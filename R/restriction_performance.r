#' Run All Restrictions of the Global Model of Discretized Rarity
#'
#' Runs the gdrare pipeline for all restrictions. Returns the list of dataframes with joined original species data, summaries of thresholds, directions, and the parameter grid.
#'
#' @param species_df Data frame of species trait and metadata.
#' @param abundance_df Data frame of site-by-species abundance data.
#' @param trait_cols Character vector of trait column names to use.
#' @param geo_methods Character vector of methods to use for regional geographic rarity. Default: c("taxonomic", "range").
#' @param fun_methods Character vector of methods to use for regional functional rarity. Default: c("min_distance", "mean_distance").
#' @param abundances Logical vector indicating whether to include abundance in local phylogenetic rarity calculations. Default: c(TRUE, FALSE).
#' @param threshold_sets List of named numeric vectors defining rarity thresholds for each combination. Must be the same length as `direction_sets`.
#' @param direction_sets List of named character vectors ("high"/"low") defining threshold directions for each combination. Must be the same length as `threshold_sets`.
#' @param species_col Name of the species identifier column in data frames. Default: "species".
#' @param use_internal_phylo Logical, whether to use an internal seed plant phylogeny. Default: TRUE.
#' @param internal_phylo_name Name of the internal seed plant phylogeny to use if applicable. Default: "ALLMB". Options: "ALLMB", "ALLOTB", "GBMB", "GBOTB".
#' @param k_means Logical, whether to apply k-means clustering. Default: FALSE.
#' @param additional_dimensions Optional list of additional custom rarity axes to include in the analysis.
#' @param model Optional specific restriction to pass through the pipeline. Default: NULL.
#' @param phylo An optional phylogenetic tree of class `phylo`. If provided and `use_internal_phylo` is `FALSE`, this tree will be used for phylogenetic rarity calculations.
#' @param verbose Logical, whether to print progress messages. Default: TRUE.
#'
#' @return A list with elements:
#' \itemize{
#'   \item results: List of data frames with rarity categorizations for restrictions per method combo.
#'   \item threshold_sets: Data frame summarizing threshold sets used.
#'   \item direction_sets: Data frame summarizing direction sets used.
#'   \item param_grid: Data frame with the full parameter grid combinations.
#' }
#' 
#' @export
run_all_gdrare_combos <- function(species_df,
                                   abundance_df = NULL,
                                   trait_cols = NULL,
                                   geo_methods = c("taxonomic", "range"),
                                   fun_methods = c("min_distance", "mean_distance"),
                                   abundances = c(TRUE, FALSE),
                                   threshold_sets = list(
                                     list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.75, PL = 0.75),
                                     list(GR = 0.15, GL = 0.15, FR = 0.75, FL = 0.75, PR = 0.75, PL = 0.75),
                                     list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.90, PL = 0.90),
                                     list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.75, PL = 0.75)
                                   ),
                                   direction_sets = list(
                                     list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                     list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                     list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                     list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high")
                                   ),
                                   species_col = "species",
                                   use_internal_phylo = TRUE,
                                   internal_phylo_name = "ALLMB",
                                   k_means = FALSE,
                                   additional_dimensions = NULL,
                                   model = NULL,
                                   phylo = NULL,
                                   verbose = TRUE) {
  
  if (length(threshold_sets) != length(direction_sets)) {
    stop("threshold_sets and direction_sets must be the same length.")
  }
  
  if (!is.null(additional_dimensions)) {
    threshold_sets <- lapply(threshold_sets, function(th) {
      for (dim in additional_dimensions) {
        if (!dim %in% names(th)) {
          th[[dim]] <- 0.85  # default threshold or user-defined
        }
      }
      th
    })
    
    direction_sets <- lapply(direction_sets, function(dir) {
      for (dim in additional_dimensions) {
        if (!dim %in% names(dir)) {
          dir[[dim]] <- "high"  # default direction
        }
      }
      dir
    })
  }
  
  param_grid <- expand.grid(
    geo_rarity_method = geo_methods,
    fun_rarity_method = fun_methods,
    abundance = abundances,
    threshold_set = seq_along(threshold_sets),
    stringsAsFactors = FALSE
  )
  
  threshold_df <- do.call(rbind, lapply(threshold_sets, as.data.frame))
  rownames(threshold_df) <- paste0("Set_", seq_along(threshold_sets))
  
  direction_df <- do.call(rbind, lapply(direction_sets, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  rownames(direction_df) <- paste0("Set_", seq_along(direction_sets))
  
  results <- vector("list", length = nrow(param_grid))
  names(results) <- paste0("combo_", seq_len(nrow(param_grid)))
  
  for (i in seq_len(nrow(param_grid))) {
    params <- param_grid[i, ]
    
    if (verbose) {
      message(paste("Running combo", i, "of", nrow(param_grid), ":",
                    params$geo_rarity_method, "+", params$fun_rarity_method,
                    "+ abundance =", params$abundance,
                    "+ threshold set", params$threshold_set))
    }
    
    phylo_to_pass <- if (use_internal_phylo) NULL else phylo
    
    result <- gdrare_pipeline(
      species_df = species_df,
      species_col = species_col,
      abundance_df = abundance_df,
      additional_dimensions = additional_dimensions,
      abundance = params$abundance,
      phylo = phylo_to_pass,
      use_internal_phylo = use_internal_phylo,
      internal_phylo_name = internal_phylo_name,
      trait_cols = trait_cols,
      use_most_complete_model = FALSE,
      geo_rarity_method = as.character(params$geo_rarity_method),
      fun_rarity_method = as.character(params$fun_rarity_method),
      k_means = k_means,
      thresholds = threshold_sets[[params$threshold_set]],
      directions = direction_sets[[params$threshold_set]],
      model = model
    )
    
    if (is.list(result) && length(result) == 1 && is.data.frame(result[[1]])) {
      result <- result[[1]]
    }
    
    results[[i]] <- result
  }
  
  rejoined_results <- lapply(results, function(res) {
    if (is.list(res) && length(res) == 1 && is.data.frame(res[[1]])) {
      res <- res[[1]]
    }
    dplyr::left_join(species_df, res, by = species_col)
  })
  
  return(list(
    results = rejoined_results,
    threshold_sets = threshold_df,
    direction_sets = direction_df,
    param_grid = param_grid
  ))
}



#' Evaluate Restriction Performance Using ANOVA
#'
#' Applies linear models for each restriction in a list of data frames, testing ability of restrictions to explain specified response variables.
#' Extracts ANOVA statistics and model summaries.
#'
#' @param df_list List of data frames each containing species data and restrictions (starting after "_flag" columns).
#' @param response_vars Character vector of response variable names to test in the linear models. Must be included in the df_list.
#'
#' @return A data frame combining ANOVA results for all models and response variables, with columns:
#' \itemize{
#'   \item method: Identifier of the combo/data frame.
#'   \item model: Restriction name.
#'   \item response_variable: Response variable tested.
#'   \item AIC: Akaike Information Criterion value for the model.
#'   \item df: Degrees of freedom used in the ANOVA.
#'   \item F_statistic: F statistic from the ANOVA.
#'   \item p_value: p-value from the ANOVA F-test.
#'   \item multiple_R2: Multiple R-squared of the model.
#'   \item adjusted_R2: Adjusted R-squared of the model.
#' }
#' 
#' @note To run highly multidimensional models such as GDR, the number of species must outnumber the possible rarity types (i.e., at least 63 species for GDR) to ensure the linear models work correctly.
#' 
#' @export
eval_restriction_performance <- function(df_list, response_vars) {
  results_list <- lapply(seq_along(df_list), function(i) {
    df <- df_list[[i]]
    combo_id <- paste0("method_", i)
    
    flag_cols <- grep("_flag$", colnames(df))
    if (length(flag_cols) == 0) stop("No columns ending in '_flag' were found.")
    
    start_col <- max(flag_cols) + 1
    model_cols <- colnames(df)[start_col:ncol(df)]
    
    df_results <- data.frame()
    
    for (model_col in model_cols) {
      for (response in response_vars) {
        formula <- as.formula(paste0(response, " ~ `", model_col, "`"))
        
        tryCatch({
          fit <- lm(formula, data = df)
          anova_result <- anova(fit)
          model_summary <- summary(fit)
          
          row <- data.frame(
            method = combo_id,
            model = model_col,
            response_variable = response,
            AIC = AIC(fit),
            df = sum(anova_result$Df, na.rm = TRUE),
            F_statistic = anova_result$`F value`[1],
            p_value = anova_result$`Pr(>F)`[1],
            multiple_R2 = model_summary$r.squared,
            adjusted_R2 = model_summary$adj.r.squared
          )
          
          df_results <- rbind(df_results, row)
        }, error = function(e) {
          warning(sprintf("Failed for model %s and response %s in %s: %s", model_col, response, combo_id, e$message))
        })
      }
    }
    
    return(df_results)
  })
  
  # Combine all results
  dplyr::bind_rows(results_list)
}


#' Get Best Methods of Applying Each Restriction Per Response Variable Based on AIC
#'
#' Groups ANOVA results by method, response variable and restriction, then selects the method with the lowest AIC for each restriction.
#' Returns a sorted data frame with best performing methods for each restriction and response variable.
#'
#' @param anova_df Data frame of ANOVA results, containing at least columns 'response_variable', 'model', and 'AIC'.
#' 
#' @return A data frame filtered to best methods per response variable and restriction, sorted by AIC.
#' 
#' @export
get_best_methods <- function(anova_df) {
  anova_df %>%
    dplyr::group_by(response_variable, model) %>%
    dplyr::arrange(AIC, .by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(response_variable, AIC)
}

#' Run Full Restriction Performance Pipeline
#'
#' Runs the full pipeline of generating gdrare combos, evaluating restriction performance through ANOVA, and summarizing the best performing methods using AIC.
#'
#' @param species_df Data frame of species trait and metadata.
#' @param abundance_df Data frame of species-by-site abundance data. Must contain columns 'species', 'site', 'presence_absence', and 'abundance'.
#' @param trait_cols Character vector of trait column names to use.
#' @param geo_methods Method to calculate geographic rarity. Options: \code{"taxonomic"} (based on site occupancy), or \code{"range"} (based on convex hull/range size). Default is \code{"taxonomic"}.
#' @param fun_methods Method for functional rarity. Options: \code{"min_distance"}, \code{"mean_distance"}, or \code{"none"} to exclude functional rarity. Default is \code{"min_distance"}.
#' @param abundances Logical; if \code{TRUE}, include abundance weighting when calculating community rarity metrics. Default is \code{TRUE}.
#' @param threshold_sets List of threshold sets for rarity classification. Must be a list of list.
#' @param direction_sets List of direction sets ("high"/"low") corresponding to threshold sets. Must be a list of list.
#' @param species_col Name of species column in species_df.
#' @param response_vars Character vector of response variables for ANOVA testing.
#' @param use_internal_phylo Logical to use internal seed plant phylogeny.
#' @param internal_phylo_name Name of the internal seed plant phylogeny to use if applicable. Default: "ALLMB". Options: "ALLMB", "ALLOTB", "GBMB", "GBOTB".
#' @param phylo An optional phylogenetic tree of class `phylo`. If provided and `use_internal_phylo` is `FALSE`, this tree will be used for phylogenetic rarity calculations.
#' @param k_means Logical, whether to use k-means clustering.
#' @param additional_dimensions Optional list of additional custom rarity axes to include in the analysis.
#' @param model Optional specified restriction to pass through.
#' @param verbose Logical to print progress. Default is \code{TRUE}.
#'
#' @return A list containing:
#' \itemize{
#'   \item combos: Output list from \code{run_all_gdrare_combos()}.
#'   \item anova_results: Data frame of combined ANOVA results.
#'   \item best_models: Data frame of best method selected per restriction and response variable.
#' }
#' 
#' @export
restriction_performance_pipeline <- function(species_df,
                                abundance_df,
                                trait_cols = c("SLA", "height", "seed_mass"),
                                geo_methods = c("taxonomic", "range"),
                                fun_methods = c("min_distance", "mean_distance"),
                                abundances = c(TRUE, FALSE),
                                threshold_sets = list(
                                  list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.75, PL = 0.75),
                                  list(GR = 0.15, GL = 0.15, FR = 0.75, FL = 0.75, PR = 0.75, PL = 0.75),
                                  list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.90, PL = 0.90),
                                  list(GR = 0.15, GL = 0.15, FR = 0.90, FL = 0.90, PR = 0.75, PL = 0.75)
                                ),
                                direction_sets = list(
                                  list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                  list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                  list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high"),
                                  list(GR = "low", GL = "low", FR = "high", FL = "high", PR = "high", PL = "high")
                                ),
                                species_col = "species",
                                response_vars = c("range_diff", "flowering_duration"),
                                use_internal_phylo = TRUE,
                                internal_phylo_name = "ALLMB",
                                phylo = NULL,
                                k_means = FALSE,
                                additional_dimensions = NULL,
                                model = NULL,
                                verbose = TRUE) {
  
  # Run all combinations
  combos <- run_all_gdrare_combos(
    species_df = species_df,
    abundance_df = abundance_df,
    additional_dimensions = additional_dimensions,
    trait_cols = trait_cols,
    geo_methods = geo_methods,
    fun_methods = fun_methods,
    abundances = abundances,
    threshold_sets = threshold_sets,
    direction_sets = direction_sets,
    species_col = species_col,
    use_internal_phylo = use_internal_phylo,
    internal_phylo_name = internal_phylo_name,
    k_means = k_means,
    model = model,
    verbose = verbose,
    phylo = phylo
  )
  
  # Evaluate restriction performance (ANOVA etc)
  anova_results <- eval_restriction_performance(
    df_list = combos$results,
    response_vars = response_vars
  )
  
  # Get best methods per response variable by AIC
  best_models <- get_best_methods(anova_results)
  
  list(
    combos = combos,
    param_grid = combos$param_grid,
    threshold_sets = combos$threshold_sets,
    direction_sets = combos$direction_sets,
    anova_results = anova_results,
    best_models = best_models
  )
}

