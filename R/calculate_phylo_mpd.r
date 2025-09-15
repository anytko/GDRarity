#' Calculate Phylogenetic Mean Pairwise Distance (MPD) per Species per Site
#'
#' This function computes the mean pairwise phylogenetic distance (MPD) for each species within sites, optionally weighting by species abundance and scaling the values relative to the maximum phylogenetic distance in the site.
#'
#' @param phy A phylogenetic tree of class phylo. Must contain all species present in df.
#' @param df A data frame containing species occurrence or abundance data. Must include columns: species, site, and presence_absence (1 for presence, 0 for absence). 
#' @param relative Logical; if TRUE (default), MPD values are scaled by the maximum pairwise distance observed in the site's phylogeny, resulting in values between 0 and 1.
#' @param weighted Logical; if TRUE (default), MPD is abundance-weighted per species. If FALSE, unweighted MPD (simple mean of distances) is calculated.
#'
#' @return A data frame that includes all rows from df augmented with a new column EU (Evolutionary Uniqueness), representing the (scaled) mean pairwise phylogenetic distance for each species within each site. Species with fewer than two co-occurring species at a site receive NA.
#'
#' @details
#' The function repeatedly prunes the phylogeny to include only species present each site. It calculates MPD for each species by averaging (or abundance-weighting) distances to other species present at the same site. Results are scaled relative to the maximum distance if relative = TRUE.
#
#' @importFrom ape drop.tip cophenetic.phylo
#'
#' @examples
#' phy <- rtree(4)
#' phy$tip.label <- c("Species1", "Species2", "Species3", "Species4")
#'
#' df <- data.frame(
#'   species = c("Species1", "Species2", "Species3", "Species4", "Species1"),
#'   site = c("SiteA", "SiteA", "SiteB", "SiteB", "SiteB"),
#'   presence_absence = c(1, 1, 1, 1, 1),
#'   abundance = c(10, 5, 3, 8, 2)
#' )
#'
#' result <- calculate_phylo_mpd(phy, df, relative = TRUE, weighted = TRUE)
#' print(result)
#' @export
calculate_phylo_mpd <- function(phy, df, relative = TRUE, weighted = TRUE) {
  # Input checks
  if (!inherits(phy, "phylo")) stop("phy must be a 'phylo' object")
  required_cols <- c("species", "site", "presence_absence")
  if (weighted) required_cols <- c(required_cols, "abundance")
  if (!all(required_cols %in% names(df))) {
    stop(paste("df must include:", paste(required_cols, collapse = ", ")))
  }

  # Filter to present species only
  df_present <- df[df$presence_absence == 1, ]

  # Prune phylogeny to species in the full dataset
  all_present_species <- unique(df_present$species)
  pruned_phy <- ape::drop.tip(phy, setdiff(phy$tip.label, all_present_species))

  # Split data by site
  site_list <- split(df_present, df_present$site)

  results <- list()

  for (site_id in names(site_list)) {
    site_df <- site_list[[site_id]]
    species_here <- unique(site_df$species)

    if (length(species_here) < 2) {
      results[[site_id]] <- data.frame(
        site = site_id,
        species = species_here,
        EU = NA_real_
      )
      next
    }

    site_phy <- ape::drop.tip(pruned_phy, setdiff(pruned_phy$tip.label, species_here))
    dist_matrix <- ape::cophenetic.phylo(site_phy)

    site_df <- site_df[site_df$species %in% rownames(dist_matrix), ]

    if (weighted) {
      # Calculate abundance-weighted MPD per species
      mpd_raw <- sapply(site_df$species, function(sp) {
        other_species <- setdiff(rownames(dist_matrix), sp)
        if (length(other_species) == 0) return(NA_real_)

        dists <- dist_matrix[sp, other_species]
        abunds <- site_df$abundance[match(other_species, site_df$species)]

        sum_d <- sum(dists * abunds, na.rm = TRUE)
        denom <- sum(abunds, na.rm = TRUE)

        if (denom == 0) return(NA_real_) else return(sum_d / denom)
      })
    } else {
      # Calculate unweighted MPD per species
      mpd_raw <- sapply(site_df$species, function(sp) {
        other_species <- setdiff(rownames(dist_matrix), sp)
        if (length(other_species) == 0) return(NA_real_)
        mean(dist_matrix[sp, other_species], na.rm = TRUE)
      })
    }

    max_dist <- if (relative) max(dist_matrix, na.rm = TRUE) else 1

    # Scale by max distance exactly like distinctiveness_com
    mpd_scaled <- mpd_raw / max_dist

    results[[site_id]] <- data.frame(
      site = site_id,
      species = site_df$species,
      EU = mpd_scaled
    )
  }

  mpd_df <- do.call(rbind, results)

  df_out <- merge(df, mpd_df, by = c("site", "species"), all.x = TRUE)

  return(df_out)
}

