#' Compute Rarity Dimensions Based on Selected Restrictions
#'
#' This function calculates multiple dimensions of rarity based on a set of rarity restrictions
#' It supports geographic, functional, and phylogenetic rarity axes across both regional and local scales, depending on the availability of trait data, abundance data, and a phylogenetic tree.
#'
#' @param models_to_run Character vector of restrictions (e.g., `"GR"`, `"FRFL"`, `"GLFRPR"`, etc.) specifying which axes to compute.
#' @param species_df Data frame containing species-level information, including traits and optionally precomputed rarity values.
#' @param species_col Name of the column in `species_df` containing species names. Default is `"species"`.
#' @param abundance_df Optional data frame with species abundance data. Must contain `"species"`, `"site"`, and `"abundance"` columns.
#' @param phylogeny Optional `phylo` object for computing phylogenetic distances.
#' @param geo_rarity_method Method for computing regional geographic rarity. Options: `"range" for range size via GBIF, or `"taxonomic"` (default) to use relative occupancy via `funrar`.
#' @param fun_rarity_method Method for regional functional rarity calculation. Options: `"min_distance"` (default) to use minimum Euclidean distance, `"mean_distance"` to use mean Euclidean distance, or `"none"`.
#' @param trait_columns Character vector of trait column names used for functional rarity metrics.
#' @param min_dbscan_points Integer for DBSCAN range estimation (minimum points). Default is 5.
#' @param min_dbscan_distance Numeric minimum distance (in degrees) for DBSCAN range estimation. Default is 1.
#' @param gbif_limit Maximum number of GBIF records to download per species (if computing range size). Default is 2000.
#' @param num_cores Number of cores for parallel operations. Default is 1.
#' @param site_col Name of the column representing community or site identity in `abundance_df`. Default is `"site"`.
#' @param abundance_col Name of the column representing abundance values. Default is `"abundance"`.
#' @param time Logical; if `TRUE`, time-sliced evolutionary distinctiveness is computed. Default is `FALSE`.
#' @param time_slices Numeric vector of time slices for calculating time-sliced ED. Used only if `time = TRUE`.
#' @param relative Logical; whether to scale phylogenetic local distinctiveness (PL) values relative to maximum distance. Default is `TRUE`.
#' @param abundance Logical; whether to use abundance-weighted PL (phylogenetic local distinctiveness). Default is `TRUE`.
#'#' @param use_precomputed_axes Logical. If \code{TRUE}, the function will use rarity axes already present in \code{species_df} 
#' for the models specified in \code{models_to_run} instead of recomputing them. 
#' Additional, non-standard axes (e.g., \code{"H"} for habitat specificity) are always 
#' passed through automatically if they exist in \code{species_df}, regardless of this setting. 
#' Default is \code{FALSE}.
#' 
#' @return A data frame containing species and their computed rarity dimensions, one column per axis (e.g., `"GR"`, `"FL"`, `"PR"`).
#'
#' @details
#' The function calculates the following axes when requested and data are available:
#' \itemize{
#'   \item \strong{GR} – Regional Geographic Rarity (range size or restrictedness)
#'   \item \strong{GL} – Local Geographic Rarity (from `funrar::scarcity_stack()`)
#'   \item \strong{FR} – Regional Functional Rarity (minimum or mean trait distance)
#'   \item \strong{FL} – Local Functional Rarity (from `funrar::distinctiveness_stack()`)
#'   \item \strong{PR} – Regional Phylogenetic Rarity (evolutionary distinctiveness)
#'   \item \strong{PL} – Local Phylogenetic Rarity (abundance-weighted or unweighted MPD)
#' }
#' User-defined custom rarity axes (e.g., `"H"` for habitat specificity) are also supported if present in `species_df`.
#'
#' Uses functions from the \pkg{funrar} package. Please cite \pkg{funrar} if this function is used in published work.
#'
#' @seealso [prepare_gdrarity_models()], [calculate_phylo_mpd()], [funrar::scarcity_stack()], [funrar::distinctiveness_stack()]
#'
#' @examples
#' \dontrun{
#' # Example with simulated species data and traits
#' species_df <- data.frame(
#'   species = c("Abies_procera", "Alnus_incana", "Carex_distans"),
#'   trait1 = c(1.1, 2.3, 3.4),
#'  trait2 = c(4.1, 3.3, 2.2)
#' )
#'
#' abundance_df <- data.frame(
#'   species = c("Abies_procera", "Alnus_incana", "Abies_procera", "Carex_distans"),
#'   site = c("A", "A", "B", "B"),
#'   presence_absence = c(1,1,1,1),
#'   abundance = c(5, 10, 3, 4)
#' )
#'
#' phylo <- ape::rtree(3)
#' phylo$tip.label <- c("Abies_procera", "Alnus_incana", "Carex_distans")
#'
#' axes <- prepare_gdrarity_axes(
#'   models_to_run = c("FR", "FL", "PR", "PL"),
#'   species_df = species_df,
#'   abundance_df = abundance_df,
#'   phylogeny = phylo,
#'   trait_columns = c("trait1", "trait2"),
#'   geo_rarity_method = "taxonomic",
#'   fun_rarity_method = "mean_distance"
#' )
#'
#' head(axes)
#' }
#' @export
prepare_gdrarity_axes <- function(
  models_to_run,
  species_df,
  species_col = "species",
  abundance_df = NULL,
  phylogeny = NULL,
  geo_rarity_method = c("taxonomic", "range"),
  fun_rarity_method = c("min_distance", "mean_distance", "none"),
  trait_columns = NULL,
  min_dbscan_points = 5,
  min_dbscan_distance = 1,
  gbif_limit = 2000,
  num_cores = 1,
  site_col = "site",
  abundance_col = "abundance",
  time = FALSE,
  time_slices = NULL,
  relative = TRUE,
  abundance = TRUE,
  use_precomputed_axes = FALSE
) {
  geo_rarity_method <- match.arg(geo_rarity_method)
  fun_rare <- match.arg(fun_rarity_method)

  message("→ fun_rare: ", fun_rare)
  message("→ trait_columns: ", paste(trait_columns, collapse = ", "))

    if (use_precomputed_axes) {
  all_axes_present <- all(models_to_run %in% names(species_df))
  if (all_axes_present) {
    # Return species_df with only species_col and requested axes (if present)
    return(
      species_df[, c(species_col, models_to_run)[c(TRUE, models_to_run %in% names(species_df))], drop = FALSE]
    )
  }
}

  # Which models to run
  run_gr <- any(grepl("GR", models_to_run))
  run_gl <- any(grepl("GL", models_to_run))
  run_fr <- any(grepl("FR", models_to_run))
  run_fl <- any(grepl("FL", models_to_run))
  run_pr <- any(grepl("PR", models_to_run))
  run_pl <- any(grepl("PL", models_to_run))

  # Determine which need computation
  needs_gr <- run_gr && !(use_precomputed_axes && "GR" %in% names(species_df))
  needs_gl <- run_gl && !(use_precomputed_axes && "GL" %in% names(species_df))
  needs_fr <- run_fr && !(use_precomputed_axes && "FR" %in% names(species_df))
  needs_fl <- run_fl && !(use_precomputed_axes && "FL" %in% names(species_df))
  needs_pr <- run_pr && !(use_precomputed_axes && "PR" %in% names(species_df))
  needs_pl <- run_pl && !(use_precomputed_axes && "PL" %in% names(species_df))


  if (needs_gl && is.null(abundance_df)) {
    stop("GL requires abundance_df.")
  }
  if (needs_fl && is.null(abundance_df)) {
    stop("FL requires abundance_df.")
  }
  if (needs_fr && is.null(abundance_df)) {
    stop("FR requires abundance_df.")
  }
  if (needs_pr && is.null(phylogeny)) {
    stop("PR requires phylogeny.")
  }
  if (needs_pl && is.null(phylogeny)) {
    stop("PL requires phylogeny.")
  }

  geographic_rarity <- NULL
  local_scarcity <- NULL
  functional_rarity <- NULL
  functional_local <- NULL
  phylogenetic_rarity <- NULL
  phylogenetic_local <- NULL
  dist_mat <- NULL

  # --- GR ---
  if (needs_gr) {
    if (geo_rarity_method == "range" && "GR" %in% names(species_df)) {
      message("→ Using precomputed GR values from input species_df.")
      geographic_rarity <- species_df %>% dplyr::select(species = !!sym(species_col), GR)
    } else if (geo_rarity_method == "range") {
      message("→ Calculating GR from GBIF range sizes.")
      gr_raw <- calc_range_size(
        data_frame = species_df,
        num_cores = num_cores,
        min_points = min_dbscan_points,
        min_distance = min_dbscan_distance,
        gbif_limit = gbif_limit,
        species_col = species_col
      )
      gr_scaled <- scale_by_median(gr_raw, columns_chosen = "range_size")
      geographic_rarity <- gr_scaled %>% dplyr::select(species, GR = range_size)
    } else {
      message("→ Calculating GR from funrar::restrictedness_stack.")
      if (!requireNamespace("funrar", quietly = TRUE)) {
        stop("Package 'funrar' is required but not installed.")
      }
      gr_vec <- funrar::restrictedness_stack(
        abundance_df,
        sp_col = "species",
        com = "site"
      )
      if (is.data.frame(gr_vec)) {
        geographic_rarity <- gr_vec %>% dplyr::rename(GR = Ri) %>% dplyr::select(species, GR)
      } else if (is.list(gr_vec)) {
        gr_vec <- unlist(gr_vec)
        geographic_rarity <- data.frame(species = names(gr_vec), GR = as.numeric(gr_vec))
      } else {
        stop("Unexpected output from funrar::restrictedness_stack()")
      }
    }
  }

  # --- GL ---
  if (needs_gl) {
    if (is.null(abundance_df)) stop("GL requires abundance_df.")
    message("Computing local scarcity (GL).")
    local_scarcity <- compute_mean_si(
      data = abundance_df,
      species_col = "species",
      site_col = "site",
      abundance_col = "abundance"
    ) %>%
      dplyr::rename(GL = Si) %>%
      dplyr::select(species, GL)
  }

  # --- Functional distances ---
  if ((needs_fl || needs_fr) && is.null(dist_mat)) {
    if (is.null(trait_columns) || length(trait_columns) < 2) {
      stop("At least two trait columns are required.")
    }
    trait_subset <- species_df %>%
      dplyr::select(species, all_of(trait_columns))
    rownames(trait_subset) <- trait_subset$species
    trait_subset <- trait_subset %>% dplyr::select(-species)
    dist_mat <- funrar::compute_dist_matrix(trait_subset)
  }

  # --- FR ---
  if (needs_fr && fun_rare != "none") {
    if (fun_rare == "min_distance") {
      message("Computing FR using minimum trait distances.")
      functional_rarity <- funrar::uniqueness_stack(
        species_df,
        sp_col = "species",
        dist_matrix = dist_mat
      ) %>% dplyr::select(species, FR = Ui)
    } else if (fun_rare == "mean_distance") {
      message("Computing FR using mean trait distances.")
      functional_rarity <- fun_dist(
        species_df,
        trait_columns = trait_columns,
        species_col = species_col
      ) %>% dplyr::select(species, FR = fun_dist)
    }
  }

  # --- FL ---
  if (needs_fl) {
    if (is.null(abundance_df)) stop("FL requires abundance_df.")
    message("Computing FL.")
    di_all <- funrar::distinctiveness_stack(
      abundance_df,
      sp_col = "species",
      com = "site",
      abund = "abundance",
      dist_matrix = dist_mat
    )
    functional_local <- di_all %>%
      dplyr::group_by(species) %>%
      dplyr::summarize(FL = mean(Di, na.rm = TRUE))
  }

  # --- PR ---
  if (needs_pr) {
    message("Computing PR.")
    pr_raw <- avg_evol_dist(
      phy = phylogeny,
      data_frame = species_df,
      time = time,
      time_slices = time_slices,
      species_col = species_col
    )
    pr_scaled <- scale_by_median(pr_raw, columns_chosen = "ED")
    phylogenetic_rarity <- pr_scaled %>% dplyr::select(species, PR = ED)
  }

  # --- PL ---
  if (needs_pl) {
    if (is.null(phylogeny)) stop("PL requires a phylogeny.")
    message("Computing PL.")
    if (is.null(abundance_df) || isFALSE(abundance)) {
      pl_df <- calculate_phylo_mpd(df = abundance_df, phy = phylogeny, relative = relative, weighted = FALSE)
    } else {
      if (!"species" %in% colnames(abundance_df)) {
        stop("abundance_df must contain a 'species' column.")
      }
      pl_df <- calculate_phylo_mpd(df = abundance_df, phy = phylogeny, relative = relative, weighted = TRUE)
    }
    phylogenetic_local <- pl_df %>%
      dplyr::group_by(species) %>%
      dplyr::summarize(PL = mean(EU, na.rm = TRUE))
  }

  # Combine results
  list_to_merge <- list(
    if (!needs_gr && "GR" %in% names(species_df)) species_df[, c(species_col, "GR")],
    if (!needs_gl && "GL" %in% names(species_df)) species_df[, c(species_col, "GL")],
    if (!needs_fr && "FR" %in% names(species_df)) species_df[, c(species_col, "FR")],
    if (!needs_fl && "FL" %in% names(species_df)) species_df[, c(species_col, "FL")],
    if (!needs_pr && "PR" %in% names(species_df)) species_df[, c(species_col, "PR")],
    if (!needs_pl && "PL" %in% names(species_df)) species_df[, c(species_col, "PL")],
    geographic_rarity,
    local_scarcity,
    functional_rarity,
    functional_local,
    phylogenetic_rarity,
    phylogenetic_local
  )

  list_to_merge <- lapply(list_to_merge, function(df) {
    if (!is.null(df)) df[[species_col]] <- as.character(df[[species_col]])
    df
  })

  list_to_merge <- list_to_merge[!sapply(list_to_merge, is.null)]
  if (length(list_to_merge) == 0) stop("No rarity axes were computed.")
  final_df <- purrr::reduce(list_to_merge, dplyr::full_join, by = species_col)

  # Custom axes
  standard_axes <- c("GR", "GL", "FR", "FL", "PR", "PL")
  custom_axes <- setdiff(models_to_run, standard_axes)
  for (axis in custom_axes) {
    if (axis %in% colnames(species_df) && !(axis %in% colnames(final_df))) {
      temp <- species_df[, c(species_col, axis)]
      final_df <- dplyr::left_join(final_df, temp, by = species_col)
    }
  }

  return(final_df)
}
