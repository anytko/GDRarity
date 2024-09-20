#' Calculate average evolutionary distinctiveness across multiple time scales
#'
#' This function calculates the average evolutionary distinctiveness for a list of species across distinct time slices.
#' 
#' @param phy A rooted, time-calibrated (branch lengths representative of time) phylogenetic tree used to calculate evolutionary distinctiveness.
#' 
#' @param data_frame A data frame containing species data with a "species_name" column.
#' 
#' @param taxon  A character vector specifying the taxa (species) of interest. Default is NULL. 
#'
#' @param time_slices A numeric vector specifying the desired time slices to consider. Default is 25 MYA, 50 MYA, 75 MYA, 100MYA, 115MYA. These should be adjusted based on the time-extent of the phylogeny; the default times are based on a large phylogeny of Angiosperms. If the chosen time slices are not reflected on the utilized phylogeny, an error will occur. 
#' 
#' @param num_cores Number of CPU cores to use for parallel processing. Default is 1. We recommend utilizing as many cores as possible.
#' 
#' @return A data frame with species names and their corresponding evolutionary distinctiveness values. Species not included in the phylogeny will return NA.
#' 
#' @details Many species are evolutionarily distinct within genera but are evolutionarily indistinct across larger phylogenetic extents. This function accounts for differences in evolutionary distinctiveness at different time extents through the calculation of evolutionary distinctiveness at five distinct phylogenetic extents. 
#' 
#' @import picante
#' @import treeio
#' @import doParallel
#' @import foreach
#' 
#' @examples 
#' 
#' Import phylogeny
#' GBMB_phy <- get_phy_angio(phy_choice = "GBMB")
#' 
#' Create dataframe with species names
#' maple_data <- data.frame(species_name = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"))
#' 
#' Calculate evolutionary distinctiveness through time (1MYA, 5MYA, 10MYA)
#' GBMB_ED <- avg_evol_dist(phy = GBMB_phy, data_frame = maple_data, num_cores=6, time_slices = c(1,5,10))
#' 
#' print(GBMB_ED)
#' 
#' @export 
avg_evol_dist <- function(phy, data_frame, taxon = NULL, time_slices = c(25, 50, 75, 100, 115), num_cores = 1) {
  # Early exit if the data_frame is empty
  if (nrow(data_frame) == 0) {
    stop("The provided data frame is empty. Cannot compute evolutionary distinctiveness.")
  }

  # Check if phy is a valid 'phylo' object
  if (!inherits(phy, "phylo")) {
    stop("The provided phy object is not of class 'phylo'.")
  }

  # Set the number of cores to use
  if (num_cores < 1) {
    stop("num_cores must be at least 1")
  }
  registerDoParallel(cores = num_cores)

  # Get unique taxon names from the data frame
  if (is.null(taxon)) {
    taxon <- unique(data_frame$species_name)
  }

  # Prune the phylogeny based on species names in the dataframe
  pruned_phy <- drop.tip(phy, setdiff(phy$tip.label, taxon))

  # Check if pruning removed all tips or the object is no longer a valid phylo object
  if (is.null(pruned_phy) || !inherits(pruned_phy, "phylo")) {
    stop("Pruning resulted in an invalid tree. Cannot compute evolutionary distinctiveness.")
  }

  # Get the age of the root of the phylogeny
  root_age <- max(ape::branching.times(pruned_phy))

  # Check if the chosen time slices are greater than the root age
  if (any(time_slices > root_age)) {
    stop("Chosen time slices are not included within the phylogeny")
  }

  # Initialize an empty list to store results
  result_list <- foreach(current_taxon = taxon, .combine = rbind) %dopar% {
    # Initialize an empty vector to store evol.dist values
    evol_dist_values <- numeric(length(time_slices))

    # Loop through each time slice
    for (j in seq_along(time_slices)) {
      # Calculate age for the current time slice
      age <- root_age - time_slices[j]

      # Slice the phylogeny
      phy_sliced <- phytools::treeSlice(pruned_phy, age, trivial = TRUE)

      # Find the focal tree
      focal_tree <- NULL
      for (i in seq_along(phy_sliced)) {
        if (current_taxon %in% phy_sliced[[i]]$tip.label) {
          focal_tree <- phy_sliced[[i]]
          break  # Exit the loop once the focal tree is found
        }
      }

      # Check if focal_tree is NULL
      if (is.null(focal_tree)) {
        evol_dist_values[j] <- NA  # Set NA for the current time slice
      } else {
        # Calculate evol.dist for the focal tree
        ed <- try(picante::evol.distinct(focal_tree, type = "fair.proportion"), silent = TRUE)

        # Store the evol.dist value for the current time slice
        if (!inherits(ed, "try-error") && length(ed$w[ed$Species == current_taxon]) > 0) {
          evol_dist_values[j] <- ed$w[ed$Species == current_taxon]
        } else {
          evol_dist_values[j] <- NA
        }
      }
    }

    # Assign NA to evol_dist_values for the current_taxon if not found in any time slice
    if (all(is.na(evol_dist_values))) {
      mean_evol_dist <- NA
    } else {
      # Calculate the mean of evol.dist values across all time slices for the current_taxon
      mean_evol_dist <- mean(evol_dist_values, na.rm = TRUE)
    }

    # Return a data frame with the results for the current_taxon
    data.frame(species_name = current_taxon, mean_evol_dist = mean_evol_dist)
  }

  # Stop the parallel backend
  stopImplicitCluster()

  # Combine the results into a data frame
  result_df <- as.data.frame(result_list)

  return(result_df)
}

