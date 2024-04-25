#' Calculate relative functional distinctiveness for species based on trait data
#'
#' This function calculates relative functional distinctiveness (FD) for species based on trait data.
#' 
#' @param data_frame A data frame containing species data with a "species_name" column.
#' 
#' @param trait_columns A character vector of trait columns used for calculating FD.
#' 
#' @import vegan
#' 
#' @details  This function scales trait data (by the median) found in a dataframe consiting of multiple traits (2+), computes Euclidean distances, and then calculates the relative functional distinctiveness of each species. This measure of functional distinctiveness is relative to the species analyzed, such that a species can be functionaly distinct within a small subset of species, but functionally indistinct across the larger tree of life.
#'
#' @return A data frame with species names and their functional distinctiveness (fun_dist) values.
#'
#' @examples
#' 
#' Create a dataframe of maple species with trait data (SLA, height, seed mass)
#' maple_data <- data.frame(species_name = c("Acer campestre", "Acer monspessulanum", "Acer negundo", "Acer opalus", "Acer platanoides", "Acer pseudoplatanus", "Acer saccharinum"), SLA = c(5, 15, 6, 11, 23.5, 52, 4), height = c(50, 65, 28, 70, 68, 43, 55), seed_mass = c(1.2, 0.57, 2, 8.5, 0.54, 2.3, 1.9))
#' 
#' FD_results <- fun_dist(data_frame = maple_data, trait_columns = c("SLA", "height", "seed_mass"))
#' 
#' print(FD_results)
#' 
#' @export
#'
fun_dist <- function(data_frame, trait_columns) {
  # Extract trait values
  trait_data <- data_frame[, trait_columns]

  # Z-transform based on the median
  trait_data <- scale(data_frame[, trait_columns], center = apply(trait_data, 2, median))

  # Calculate Euclidean distances
  distances <- vegan::vegdist(trait_data, method = "euclidean")

  # Calculate functional distinctiveness
  fd_values <- rowMeans(as.matrix(distances))

  # Create a new dataframe with species names and FD values
  result_df <- data.frame(species_name = data_frame$species_name, fun_dist = fd_values)

  # Join the FD values to the initial dataset
  joined_data <- merge(data_frame, result_df, by = "species_name", all.x = TRUE)

  return(joined_data)
}


