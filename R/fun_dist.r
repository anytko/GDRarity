#' Calculate Relative Functional Distinctiveness (FD) from Trait Data
#'
#' Computes relative functional distinctiveness for species based on multiple traits. 
#' Traits are median-centered before computing pairwise Euclidean distances, and 
#' FD is calculated as the mean distance of each species to all others.
#' 
#' @param data_frame A data frame containing species data with a column of species names.
#' 
#' @param trait_columns Character vector specifying the names of numeric trait columns to use for FD calculation. Must contain at least two traits.
#' 
#' @param species_col Character string specifying the column in `data_frame` that contains species names. Default is `"species"`.
#' 
#' @import vegan
#' @importFrom dplyr rename
#' @importFrom rlang ensym
#' 
#' @details  Functional distinctiveness (FD) for each species 
#' is calculated as the mean of its distances to all other species in the dataset.  
#' This is a relative measure — a species can be functionally distinct within a subset 
#' of species but not necessarily across all species globally.
#'
#' @return A `data.frame` with one row per species containing:
#'   \itemize{
#'     \item `species_col` — species name.
#'     \item `fun_dist` — functional distinctiveness value.
#'   }
#'
#' @examples
#' 
#' # Create a dataframe of maple species with trait data (SLA, height, seed mass)
#' maple_data <- data.frame(species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", "Acer saccharinum"), SLA = c(5, 15, 6, 11, 23.5, 52, 4), height = c(50, 65, 28, 70, 68, 43, 55), seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9))
#' 
#' FD_results <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
#' 
#' print(FD_results)
#' 
#' @export
fun_dist <- function(data_frame, trait_columns, species_col = "species") {
  if (nrow(data_frame) == 0) {
    stop("Error: The data frame is empty.")
  }

  if (length(trait_columns) < 2) {
    stop("Error: At least 2 traits are required for functional distinctiveness calculation.")
  }

  # Capture species column using tidy evaluation
  species_sym <- rlang::ensym(species_col)

  # Temporarily rename for internal consistency
  data_frame <- data_frame %>%
    dplyr::rename(species_name = !!species_sym)

  trait_data <- data_frame[, trait_columns]
  trait_data <- scale(trait_data, center = apply(trait_data, 2, median))

  distances <- vegan::vegdist(trait_data, method = "euclidean")
  fd_values <- rowMeans(as.matrix(distances))

  # Return original species_col name in final output
  output <- data.frame(species_name = data_frame$species_name, fun_dist = fd_values)
  names(output)[names(output) == "species_name"] <- species_col
  return(output)
}





