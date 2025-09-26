#' Prepare Restrictions for the Global Model of Discretized Rarity
#'
#' This function assesses available data for a given set of species and determines which rarity dimensions and corresponding restrictions can be used to analyze multidimensional rarity. It also supports automated model selection based on data completeness.
#'
#' @param species_df A data frame with at least one column identifying species (default column name is `"species"`).
#' @param abundance_df Optional data frame with abundance data. Must contain columns `"species"`, `"site"`, and `"abundance"` if provided.
#' @param phylo Optional `phylo` object representing a phylogenetic tree. If not provided, an internal tree can be used.
#' @param use_internal_phylo Logical; if `TRUE` (default) and no `phylo` is provided, the function will attempt to retrieve an internal seed plant phylogeny.
#' @param internal_phylo_name Character; name of the internal tree to use (e.g., `"ALLMB"`).
#' @param trait_cols Optional character vector specifying the names of trait columns in `species_df`.
#' @param species_col Character; the name of the column in `species_df` that contains species names (default `"species"`).
#' @param use_most_complete_model Logical; if `TRUE`, returns only the most complete model available.
#' @param model Optional character vector of specific model codes to restrict the output to. If specified, only these models are returned (if valid). Restrictions are named using rarity dimensions in the following order: geographic (G), functional (F), and phylogenetic (P) at regional (R) and local (L) scales; to run the full global model the code is GRGLFRFLPRPL.
#' @param additional_dimensions Optional character vector of additional column names in `species_df` to treat as rarity dimensions (e.g., `"HabitatType"`).
#'
#' @return An (invisible) list with the following components:
#' \describe{
#'   \item{species_df}{The processed species data frame.}
#'   \item{abundance_df}{The original abundance data frame if provided.}
#'   \item{trait_cols}{The trait column names, if traits were available.}
#'   \item{phylo}{The pruned phylogenetic tree used, or `NULL` if none available.}
#'   \item{available_axes}{Character vector of available rarity axes (e.g., `"GR"`, `"FR"`).}
#'   \item{models_to_run}{Character vector of restrictions to use based on input data.}
#' }
#'
#' @details
#' The function checks for availability of different rarity dimensions:
#' \itemize{
#'   \item `"GR"`: Geographic restriction (always available if species list exists).
#'   \item `"GL"`: Geographic local abundance (requires `abundance_df`).
#'   \item `"FR"`: Functional restriction (requires trait data).
#'   \item `"FL"`: Functional local abundance (requires trait and abundance data).
#'   \item `"PR"`: Phylogenetic restriction (requires a phylogeny).
#'   \item `"PL"`: Phylogenetic local abundance (requires a phylogeny).
#' }
#'
#' Additional rarity dimensions can be provided. The function  constructs valid model codes (e.g., `"GRFR"`, `"GRGLFRFL"`, etc.) based on combinations 
#' of available axes.
#'
#' @seealso [get_phy_angio()] for retrieving internal phylogenies.
#'
#' @importFrom ape drop.tip
#' 
#' @examples
#' \dontrun{
#' species_df <- data.frame(species = c("Abies_procera", "Alnus_incana"), trait1 = c(1.2, 3.4))
#' abundance_df <- data.frame(species = c("Abies_procera", "Alnus_incana"), site = c("A", "A"), abundance = c(10, 5))
#' 
#' setup <- prepare_gdrarity_models(
#'   species_df = species_df,
#'   abundance_df = abundance_df,
#'   trait_cols = "trait1",
#'   use_internal_phylo = TRUE,
#'   internal_phylo_name = "ALLMB"
#' )
#' setup$models_to_run
#' }
#'
#' @export
prepare_gdrarity_models <- function(species_df,
                                    abundance_df = NULL,
                                    phylo = NULL,
                                    use_internal_phylo = TRUE,
                                    internal_phylo_name = "ALLMB",
                                    trait_cols = NULL,
                                    species_col = "species",
                                    use_most_complete_model = FALSE,
                                    model = NULL,
                                    additional_dimensions = NULL) {

  # ---- Step 1: Standardize species column ----
  if (!(species_col %in% names(species_df))) {
    stop("Species column not found in species_df. Please check 'species_col' argument.")
  }
  species_df$species <- species_df[[species_col]]
  
  # ---- Step 2: Phylogeny handling ----
  phylo_available <- FALSE
  
  if (is.null(phylo)) {
    if (use_internal_phylo) {
      message("🧬 Retrieving internal seed plant phylogeny...")
      internal_phylo <- get_phy_angio(internal_phylo_name)
      matched_species <- intersect(species_df$species, internal_phylo$tip.label)
      
      if (length(matched_species) >= 1) {
        phylo <- ape::drop.tip(internal_phylo, setdiff(internal_phylo$tip.label, matched_species))
        phylo_available <- TRUE
      } else {
        warning("No species matched internal phylogeny. Phylogenetic rarity will not be calculated.")
      }
    }
  } else {
    if (!inherits(phylo, "phylo")) stop("Provided phylogeny must be of class 'phylo'.")
    matched_species <- intersect(species_df$species, phylo$tip.label)
    
    if (length(matched_species) >= 1) {
      phylo <- ape::drop.tip(phylo, setdiff(phylo$tip.label, matched_species))
      phylo_available <- TRUE
    } else {
      warning("None of the species match the provided phylogeny. Phylogenetic rarity will not be calculated.")
    }
  }

  # ---- Step 3: Trait and abundance checks ----
  trait_available <- !is.null(trait_cols) && all(trait_cols %in% names(species_df))
  abundance_available <- !is.null(abundance_df) &&
    all(c("species", "site", "abundance") %in% names(abundance_df))
  
  # ---- Step 4: Determine available axes ----
  axis_flags <- list(
    GR = TRUE,  # Always available from GBIF
    GL = abundance_available,
    FR = trait_available,
    FL = trait_available && abundance_available,
    PR = phylo_available,
    PL = phylo_available && abundance_available
  )
  

  # ---- Step 5: Build dimension table ----
  dimension_groups <- list(
    G = c("GR", "GL")[c(axis_flags$GR, axis_flags$GL)],
    F = c("FR", "FL")[c(axis_flags$FR, axis_flags$FL)],
    P = c("PR", "PL")[c(axis_flags$PR, axis_flags$PL)]
  )

  # ---- Step 5b: Add user-supplied dimensions if present ----
  if (!is.null(additional_dimensions)) {
    if (!all(additional_dimensions %in% names(species_df))) {
      stop("One or more additional_dimensions not found in species_df.")
    }
    extra_codes <- sapply(additional_dimensions, function(col) {
      first_letter <- toupper(substr(col, 1, 1))
      if (first_letter %in% c("G", "F", "P")) {
        toupper(substr(col, 1, 2))
      } else {
        first_letter
      }
    })
    dimension_groups$X <- extra_codes
  }

  # ---- Step 6: Model logic based on completeness ----
  available_axes <- unlist(dimension_groups)
  
  generate_model_names <- function(groups) {
    all_combos <- unlist(lapply(1:length(groups), function(k) {
      combn(groups, k, simplify = FALSE)
    }), recursive = FALSE)

    axis_order <- c("G", "F", "P", "X")  # Include extra dimension group
    scale_order <- c("R", "L")

    sorted_combos <- sapply(all_combos, function(x) {
      axes <- substring(x, 1, 1)
      scales <- substring(x, 2, 2)
      order_idx <- order(match(axes, axis_order), match(scales, scale_order, nomatch = 3))
      paste(x[order_idx], collapse = "")
    }, USE.NAMES = FALSE)

    unique(sorted_combos)
  }

  if (use_most_complete_model) {
    full_combo <- unique(available_axes)
    axes <- substring(full_combo, 1, 1)
    scales <- substring(full_combo, 2, 2)
    
    axis_order <- c("G", "F", "P", "X")
    scale_order <- c("R", "L")
    order_idx <- order(match(axes, axis_order), match(scales, scale_order, nomatch = 3))
    
    model_name <- paste(full_combo[order_idx], collapse = "")
    model_list <- model_name
  } else {
    model_list <- generate_model_names(available_axes)
  }

  filtered_models <- model_list

  # ---- Step 7: If specific model was requested, validate it ----
  if (!is.null(model)) {
    missing_models <- setdiff(model, filtered_models)
    valid_models <- intersect(model, filtered_models)

    if (length(valid_models) == 0) {
      stop(sprintf(
        "None of the specified models (%s) are available given current data input.\n Available models: %s",
        paste(model, collapse = ", "),
        paste(filtered_models, collapse = ", ")
      ))
    }

    if (length(missing_models) > 0) {
      warning(sprintf(
        "Some specified models were not available and will be ignored: %s\n Using available models: %s",
        paste(missing_models, collapse = ", "),
        paste(valid_models, collapse = ", ")
      ))
    }

    filtered_models <- valid_models
  }

  # ---- Step 8: Return metadata and model list ----
    extra_axes <- if (!is.null(additional_dimensions)) {
    sapply(additional_dimensions, function(col) {
      first_letter <- toupper(substr(col, 1, 1))
      if (first_letter %in% c("G", "F", "P")) {
        toupper(substr(col, 1, 2))
      } else {
        first_letter
      }
    })
  } else character(0)

  species_df_out <- species_df  # copy to avoid editing original

  if (!is.null(additional_dimensions)) {
    custom_axis_map <- setNames(extra_axes, additional_dimensions)
    for (orig_col in names(custom_axis_map)) {
      new_col <- custom_axis_map[[orig_col]]
      species_df_out[[new_col]] <- species_df_out[[orig_col]]
    }
  }

  return(invisible(list(
    species_df = species_df_out,
    abundance_df = abundance_df,
    trait_cols = if (trait_available) trait_cols else NULL,
    phylo = if (phylo_available) phylo else NULL,
    available_axes = c(names(Filter(identity, axis_flags)), extra_axes),
    models_to_run = unique(filtered_models)
  )))
}
