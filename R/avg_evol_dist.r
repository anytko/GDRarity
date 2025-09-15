#' Calculate average evolutionary distinctiveness (ED) for a set of species
#'
#' Computes the average evolutionary distinctiveness (ED) of each species based on a rooted, time-calibrated phylogenetic tree. ED can be calculated either across the full tree or across multiple evolutionary time slices.
#'
#' @param phy A rooted, time-calibrated phylogenetic tree (`phylo` object). 
#'   Branch lengths must reflect time (e.g., millions of years).
#'
#' @param data_frame A data frame containing the species of interest. Must include a column with species names.
#'
#' @param species_col A character string specifying the name of the species column in data_frame. Default is "species".
#'
#' @param taxon Optional character vector specifying which taxa (species) to compute ED for. If NULL (default), all unique species in data_frame are used.
#'
#' @param time Logical; if FALSE, ED is calculated across the full tree. If TRUE, ED is calculated across multiple evolutionary time slices. Default is TRUE
#'
#' @param time_slices Numeric vector of time depths (in millions of years ago, MYA) at which to slice the tree. Default is \code{c(25, 50, 75, 100, 115)}. Values must be less than the root age of `phy`. Ultrametric trees are recommended.
#'
#' @param num_cores Number of CPU cores to use for parallel processing. Default is 1. We recommend increasing this for faster computation.
#'
#'
#' @details 
#' Evolutionary distinctiveness (ED) is computed using the `"fair.proportion"`method from **picante**. When `time = TRUE`, the phylogeny is sliced at each time depth using [phytools::treeSlice()] and ED is computed for each slice. 
#' The mean ED across slices is returned for each species.
#' 
#' Calculating ED at several evolutionary depths captures how distinct species are over deep vs. shallow time scales.
#'
#' Parallel processing is supported via **foreach** and **doParallel**.
#' 
#' @return A `data.frame` with two columns:  
#'   1. The species column (name matches `species_col`).  
#'   2. `ED` — the average evolutionary distinctiveness.
#'
#' @import picante
#' @import treeio
#' @import phytools
#' @importFrom ape drop.tip branching.times rtree
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#'
#' @examples
#' # Generate a phylogeny
#' set.seed(123)
#' random_phy <- ape::rtree(n = 7)
#' random_phy$tip.label <- c("Acer_campestre", "Acer_monspessulanum",
#'                           "Acer_negundo", "Acer_opalus",
#'                           "Acer_platanoides", "Acer_pseudoplatanus",
#'                           "Acer_saccharinum")
#'
#' # Create a dataframe of species
#' maple_data <- data.frame(species = random_phy$tip.label)
#'
#' # Compute ED using time slices
#' ed_results <- avg_evol_dist(phy = random_phy,
#'                             data_frame = maple_data,
#'                             species_col = "species",
#'                             num_cores = 1,
#'                             time_slices = c(0.5, 1, 1.5))
#'
#' print(ed_results)
#'
#' @export
avg_evol_dist <- function(phy, data_frame, species_col = "species", taxon = NULL, time = TRUE, 
                          time_slices = c(25, 50, 75, 100, 115), 
                          num_cores = 1) {
  # Early exit if the data_frame is empty
  if (nrow(data_frame) == 0) {
    stop("The provided data frame is empty. Cannot compute evolutionary distinctiveness.")
  }

  if (!inherits(phy, "phylo")) {
    stop("The provided phy object is not of class 'phylo'.")
  }
  
  # Warn if tree is not ultrametric and time slicing is requested
  if (time && !ape::is.ultrametric(phy)) {
    warning("The phylogeny is not ultrametric. Some species may not be present at chosen time slices and will return NA.")
  }

  if (num_cores < 1) stop("num_cores must be at least 1")
  registerDoParallel(cores = num_cores)

  species_vec <- data_frame[[species_col]]

  if (is.null(taxon)) {
    taxon <- unique(species_vec)
  }

  # Prune the tree
  pruned_phy <- ape::drop.tip(phy, setdiff(phy$tip.label, taxon))

  if (is.null(pruned_phy) || !inherits(pruned_phy, "phylo")) {
    stop("Pruning resulted in an invalid tree.")
  }

  root_age <- max(ape::branching.times(pruned_phy))

  if (time && any(time_slices > root_age)) {
    stop("Chosen time slices exceed the root age of the phylogeny.")
  }

  if (time) {
    # With time slicing
    result_list <- foreach(current_taxon = taxon, .combine = rbind) %dopar% {
      evol_dist_values <- numeric(length(time_slices))

    for (j in seq_along(time_slices)) {
      if (time_slices[j] == 0) {
        # Time slice at present (tips) — use full pruned tree directly
        focal_tree <- pruned_phy
      } else {
        age <- root_age - time_slices[j]
        phy_sliced <- phytools::treeSlice(pruned_phy, age, trivial = TRUE)

        focal_tree <- NULL
        for (i in seq_along(phy_sliced)) {
          if (current_taxon %in% phy_sliced[[i]]$tip.label) {
            focal_tree <- phy_sliced[[i]]
            break
          }
        }
      }

      if (is.null(focal_tree)) {
        evol_dist_values[j] <- NA
      } else {
        ed <- try(picante::evol.distinct(focal_tree, type = "fair.proportion"), silent = TRUE)
        if (!inherits(ed, "try-error") && current_taxon %in% ed$Species) {
          evol_dist_values[j] <- ed$w[ed$Species == current_taxon]
        } else {
          evol_dist_values[j] <- NA
        }
      }
    }


      mean_evol_dist <- if (all(is.na(evol_dist_values))) NA else mean(evol_dist_values, na.rm = TRUE)
      data.frame(species_name = current_taxon, ED = mean_evol_dist)
    }

  } else {
    # Without time slicing
    ed <- try(picante::evol.distinct(pruned_phy, type = "fair.proportion"), silent = TRUE)
    if (inherits(ed, "try-error")) {
      stop("Error computing evol.distinct on full tree.")
    }

    result_list <- foreach(current_taxon = taxon, .combine = rbind) %dopar% {
      if (current_taxon %in% ed$Species) {
        mean_evol_dist <- ed$w[ed$Species == current_taxon]
      } else {
        mean_evol_dist <- NA
      }
      data.frame(species_name = current_taxon, ED = mean_evol_dist)
    }
  }

  stopImplicitCluster()
  result_df <- as.data.frame(result_list)

  # Rename species column to match species_col
  colnames(result_df)[1] <- species_col

  return(result_df[, c(species_col, "ED")])
}
