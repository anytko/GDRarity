#' Compute Mean Scarcity Index (Si) for Each Species
#'
#' This function calculates the average scarcity index (Si) for each species across sites using relative abundance data and the \pkg{funrar} package's `scarcity_stack()` function.
#'
#' It first computes relative abundances within each site, applies `scarcity_stack()` from the \pkg{funrar} package, and then averages the Si values for each species.
#'
#' @param data A data frame containing species, site, and abundance information.
#' @param species_col Name of the column in `data` indicating species identity. Defaults to `"species"`.
#' @param site_col Name of the column in `data` indicating site identity. Defaults to `"site"`.
#' @param abundance_col Name of the column in `data` containing species abundance per site. Defaults to `"abundance"`.
#'
#' @return A data frame with species and their corresponding mean Si values.
#'
#' @importFrom dplyr group_by summarize
#' 
#' @details
#' This function uses `funrar::scarcity_stack()` to compute the scarcity index (Si) per species per site based on relative abundance, and then returns the average Si per species.
#'
#' @note
#' This function requires the \pkg{funrar} package. If you use this function in published work, please cite the \pkg{funrar} package using: \code{citation("funrar")}.
#'
#' @examples
#'
#' df <- data.frame(
#'   species = c("sp1", "sp2", "sp1", "sp3", "sp2", "sp3"),
#'   site = c("A", "A", "B", "B", "C", "C"),
#'   abundance = c(10, 5, 3, 8, 2, 7)
#' )
#' compute_mean_si(df)
#' 
#' @export
compute_mean_si <- function(data, species_col = "species", site_col = "site", abundance_col = "abundance") {
  # Load required package
  if (!requireNamespace("funrar", quietly = TRUE)) {
    stop("The 'funrar' package is required but not installed.")
  }

  # Step 1: Compute total abundance per site
site_abundances <- aggregate(
  reformulate(site_col, response = abundance_col),
  data = data,
  FUN = sum
)
  colnames(site_abundances)[2] <- "total_abundance"

  # Step 2: Merge back with original data
  data_merged <- merge(data, site_abundances, by = site_col)

  if (all(data_merged[[abundance_col]] == 0)) {
    stop("Scarcity cannot be computed because all abundance values are zero.")
  }

  # Step 3: Compute relative abundance
  data_merged$rel_abund <- data_merged[[abundance_col]] / data_merged$total_abundance

  # Step 4: Compute scarcity index (Si)
  si_df <- funrar::scarcity_stack(
    data_merged,
    sp_col = species_col,
    com = site_col,
    abund = "rel_abund"
  )

  # Step 5: Average Si per species
  mean_si <- si_df %>%
    group_by(.data[[species_col]]) %>%
    summarize(Si = mean(Si, na.rm = TRUE), .groups = "drop")

  return(mean_si)
}
