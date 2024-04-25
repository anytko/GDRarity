#' Define Custom Ranges
#' 
#' Perform k-means clustering on a variable and define custom ranges based on cluster centers.
#' 
#' @param data A data frame containing the variable for clustering.
#' @param variable The name of the variable for clustering. Use the name of the column in "".
#' @param k The number of clusters to be used in k-means clustering.
#' 
#' @return A list containing: A data frame with an additional column indicating the cluster label for each observation.
#' 
#' @details This function performs k-means clustering on a variable in the provided data frame and defines custom ranges based on the cluster centers. It returns a list containing the modified data frame with an additional column indicating the cluster label for each observation, and a list of custom ranges based on the cluster centers.
#' 
#' @examples
#' 
#' Create a dataframe of maple, ash, and pine species with range size, evolutionary distinctiveness, and functional distinctiveness values
#' 
#' species_names <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#' 
#' FD_values <- runif(min = -2, max = 2, n=23)
#' 
#' range_values <- runif(min = -2, max = 2, n=23)
#' 
#' mean_evol_dist_values <- runif(min = -2, max = 2, n=23)
#'
#' forest_data <- data.frame(species_name = species_names, fun_dist = FD_values, range_size = range_values, mean_evol_dist = mean_evol_dist_values)
#' 
#' Calculate the range of each k cluster
#' 
#' k_ranges <- define_custom_ranges(data = forest_data, variable = "range_size", k=8)
#'
#' print(k_ranges)
#' 
#' @export
#' 
define_custom_k_ranges <- function(data, variable, k) {
  # Perform k-means clustering
  clusters <- kmeans(data[[variable]], centers = k)

  # Extract cluster labels
  cluster_labels <- clusters$cluster

  # Obtain cluster centers
  cluster_centers <- clusters$centers

  # Initialize a list to store custom ranges
  custom_ranges <- list()

  # Sort cluster centers
  sorted_centers <- sort(cluster_centers)

  # Define custom ranges based on cluster centers
  for (i in seq_along(sorted_centers)) {
    if (i == 1) {
      lower_bound <- -Inf
    } else {
      lower_bound <- sorted_centers[i - 1]
    }
    upper_bound <- sorted_centers[i]
    range_name <- paste("Cluster", i, sep = "_")
    custom_ranges[[range_name]] <- c(lower_bound, upper_bound)
  }

  # Add cluster labels to the original data
  data$Cluster <- factor(cluster_labels)

  # Return the data with custom cluster ranges
  return(list(data = data, custom_ranges = custom_ranges))
}
