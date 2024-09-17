#' Plot clipped convex hulls on map
#' 
#' @import leaflet
#' @import sf
#' 
#' @param clipped_convex_hulls A list of clipped convex hulls for each species.
#' 
#' @details This function plots a list of clipped convex hulls onto a world leaflet map. 
#' 
#' @examples 
#' data <- data.frame(species_name = c("Eucalyptus gunnii", "Eucalyptus tenuiramis", "Eucalyptus globulus"))
#' continent_bounds <- get_continent_sf()
#' euc_convex_hulls <- get_range_convex_hulls(data)
#' euc_clipped_hulls <- clip_polygons_to_land(euc_convex_hulls, continent_bounds)
#' plot_clipped_hulls(clipped_convex_hulls = euc_clipped_hulls)
#'
#' @export
#' 
plot_clipped_hulls <- function(clipped_convex_hulls) {
  # Extract species names from the names of the list
  species_names <- names(clipped_convex_hulls)
  
  # Define a color palette with more flexibility
  n_species <- length(species_names)
  colors <- RColorBrewer::brewer.pal(min(n_species, 12), "Set1")  # Use up to 12 colors from the Set1 palette
  
  if (n_species > 12) {
    # If more than 12 species, generate additional colors
    additional_colors <- colorRampPalette(brewer.pal(12, "Set1"))(n_species - 12)
    colors <- c(colors, additional_colors)
  }
  
  # Create a leaflet map
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
  
  # Initialize a vector to hold legend labels and colors
  legend_labels <- species_names
  legend_colors <- colors[1:n_species]
  
  # Add clipped polygons for each species
  for (j in seq_along(clipped_convex_hulls)) {
    species_polygons <- clipped_convex_hulls[[j]]
    species_name <- species_names[j]
    color <- colors[j]
    
    # Create an empty list to hold valid sf objects
    sf_polygons <- list()
    
    # Convert each sfc object to sf object and add to the list
    for (i in seq_along(species_polygons)) {
      polygon_sfc <- species_polygons[[i]]
      
      # Convert sfc to sf object
      if (!is.null(polygon_sfc)) {
        polygon_sf <- st_as_sf(st_sfc(polygon_sfc, crs = 4326))
        sf_polygons[[i]] <- polygon_sf
      }
    }
    
    # Combine all sf polygons into a single sf object
    if (length(sf_polygons) > 0) {
      combined_sf <- do.call(rbind, sf_polygons)
      
      # Add polygons to the map
      m <- m %>% addPolygons(
        data = combined_sf,
        color = color,
        fillOpacity = 0.5,
        weight = 1,
        group = species_name  # Add group for easy legend management
      )
    }
  }
  
  # Add legend to the map
  m <- m %>%
    addLegend(
      position = "bottomright",
      colors = legend_colors,
      labels = legend_labels,
      title = "Species"
    )
  
  # Print the map
  print(m)
}




