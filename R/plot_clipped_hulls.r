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
  # Check if the input is empty
  if (length(clipped_convex_hulls) == 0) {
    # Create an empty leaflet map
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
    print(m)
    return(m)  # Return the empty map
  }
  
  # Extract species names from the names of the list
  species_names <- names(clipped_convex_hulls)
  
  # Define a color palette
  n_species <- length(species_names)
  colors <- RColorBrewer::brewer.pal(min(n_species, 12), "Set1")
  
  if (n_species > 12) {
    # Generate additional colors if more than 12 species
    additional_colors <- colorRampPalette(brewer.pal(12, "Set1"))(n_species - 12)
    colors <- c(colors, additional_colors)
  }
  
  # Create a leaflet map
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
  
  # Initialize lists to hold valid species and polygons
  valid_species <- list()
  valid_colors <- list()
  
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
      
      if (!is.null(polygon_sfc)) {
        polygon_sf <- st_as_sf(st_sfc(polygon_sfc, crs = 4326))
        sf_polygons[[i]] <- polygon_sf
      }
    }
    
    # If there are valid polygons, add them to the map and the valid lists
    if (length(sf_polygons) > 0) {
      combined_sf <- do.call(rbind, sf_polygons)
      
      m <- m %>% addPolygons(
        data = combined_sf,
        color = color,
        fillOpacity = 0.5,
        weight = 1,
        group = species_name
      )
      
      valid_species <- c(valid_species, species_name)
      valid_colors <- c(valid_colors, color)
    }
  }
  
  # Add legend if there are valid species
  if (length(valid_species) > 0) {
    m <- m %>%
      addLegend(
        position = "bottomright",
        colors = unlist(valid_colors),
        labels = unlist(valid_species),
        title = "Species"
      )
  }
  
  # Print the map
  print(m)
  return(m)
}






