#' Plot range polygons on world map
#' 
#' @param species_name A character vector of species names.
#' @param min_points The minimum number of points required to form a range cluster. Default is 5.
#' @param min_distance The minimum distance between points in a range cluster. Default is 1 epsilon (eps).
#' @param gbif_limit The maximum number of GBIF records to retrieve per species. Default is 2000.
#' @param plot Logical indicating whether to plot the convex hulls on a map.
#' @return A leaflet map of species range polygons.
#' @import leaflet
#' 
#' @details This function combines the functionality of EcoStatusR::create_range_polygons() and EcoStatusR::plot_convex_hulls(). Specifcally this function creates convex hulls for species using occurence data from GBIF and plots them onto a world leaflet map. 
#' 
#' @examples 
#' 
#' species_data <- data.frame(species_name = c("Abies alba", "Quercus nigra"))
#'
#' # Plotting unclipped range polygons
#' range_poly_map(data_frame = species_data)
#' 
#' # Plotting clipped range polygons
#' continent_bounds <- get_continent_sf()
#' range_poly_map(data_frame = species_data, clip = TRUE, continent_sf = continent_bounds)
#'
#' @export
#' 
range_poly_map <- function(data_frame, species_name = NULL, min_points = 5, min_distance = 1, gbif_limit = 2000, plot = TRUE, clip = FALSE, continent_sf = NULL) {
  
  # Call create_range_polygons function
  convex_hulls_list <- get_range_convex_hulls(data_frame = data_frame, species_name = species_name, min_points = min_points, min_distance = min_distance, gbif_limit = gbif_limit)
  
  # Clip polygons if clip is TRUE and continent_sf is provided
  if (clip) {
    if (is.null(continent_sf)) {
      stop("continent_sf must be provided when clip = TRUE")
    }
    convex_hulls_list_clipped <- clip_polygons_to_land(convex_hulls_list, continent_sf)

    # Plot the clipped polygons
    if (plot) {
      plot_clipped_hulls(convex_hulls_list_clipped)
    }
  } else {
    # Plot the convex hulls
    if (plot) {
      plot_convex_hulls(convex_hulls_list)
    }
  }
}

