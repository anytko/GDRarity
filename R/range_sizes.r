#' Compute range sizes (sq km) from clipped or raw range polygons
#'
#' This function calculates the range size (sq km) of species range polygons.
#'
#' @param clipped_polygons_list A list of clipped range polygons.
#' @import sf
#' 
#' @examples
#' # Create a dataframe with a 'species_name' column
#' test_data <- data.frame(species_name = c("Acer_campestre", "Acer_platanoides"))
#' # Generate range polygons for the speceis in the dataframe
#' polygon_list <- get_range_convex_hulls(test_data)
#' # Get continent boundaries
#' continent_sf <- get_continent_sf()
#' # Clip these range polygons to the continent bounds
#' clipped_polygon_list <- clip_polygons_to_land(polygon_list, continent_sf)
#' # Generate range sizes for each speices using the clipped range polygons
#' sizes <- range_sizes(clipped_polygons_list = clipped_polygon_list)
#' @export
#' 
range_sizes <- function(clipped_polygons_list) {
  if (is.null(clipped_polygons_list) || length(clipped_polygons_list) == 0) {
    return(data.frame(species_name = character(), range_size = numeric(), stringsAsFactors = FALSE))
  }
  
  # Initialize a list to store area sizes for each species
  species_range_sizes <- list()

  # Iterate over each species in the clipped polygons list
  for (species_name in names(clipped_polygons_list)) {
    # Initialize area accumulator for this species
    total_area <- 0
    
    # Iterate over each clipped polygon for this species
    clipped_polygons <- clipped_polygons_list[[species_name]]
    for (clipped_polygon in clipped_polygons) {
      tryCatch({
        # Check if the clipped polygon is valid
        if (st_is_valid(clipped_polygon)) {
          # Calculate the area and add it to the total_area for this species
          area <- st_area(clipped_polygon) / 1e6  # Convert to square kilometers
          total_area <- total_area + as.numeric(area)
        }
      }, error = function(e) {
        # Handle invalid geometries
        warning("Error calculating area for clipped polygon: ", conditionMessage(e))
      })
    }
    
    # Store the total area for this species in a data frame
    species_range_sizes[[length(species_range_sizes) + 1]] <- data.frame(
      species_name = species_name,
      range_size = total_area,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine the individual data frames into one
  result_df <- do.call(rbind, species_range_sizes)
  
  # Reset row names to avoid species names being used as row names
  rownames(result_df) <- NULL
  
  return(result_df)
}

