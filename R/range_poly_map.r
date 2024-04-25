#' Plot range polygons on world map
#' 
#' @param species_name A character vector of species names.
#' @param min_points Minimum number of points to form a cluster using dbscan.
#' @param min_distance Minimum distance between points in the same cluster.
#' @param gbif_limit Maximum number of records to fetch from GBIF.
#' @param plot Logical indicating whether to plot the convex hulls on a map.
#' @return A list of convex hulls for each species, or NULL if no data is found.
#' @import leaflet
#' 
#' @details This function combines the functionality of EcoStatusR::create_range_polygons() and EcoStatusR::plot_convex_hulls(). Specifcally this function creates convex hulls for species using occurence data and plots them onto a world leaflet map. 
#' 
#' @examples 
#' 
#' data <- data.frame(species_name = c("Quercus alba"))
#'
#' range_poly_map(data_frame = data)
#'
#' @export

range_poly_map <- function(data_frame, species_name = NULL, min_points = 5, min_distance = 1, gbif_limit = 2000, plot = TRUE) {
  # Call create_range_polygons function
  convex_hulls_list <- create_range_polygons(data_frame = data_frame, species_name = species_name, min_points = min_points, min_distance = min_distance, gbif_limit = gbif_limit)
  
  if (plot) {
    # Call plot_convex_hulls function
    plot_convex_hulls(convex_hulls_list)
  }
  
}


