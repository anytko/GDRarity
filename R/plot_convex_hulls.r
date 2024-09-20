#' Plot convex hulls on map
#' 
#' @import leaflet
#' @import sf
#' 
#' @param convex_hulls A list of convex hulls for each species.
#' 
#' @details This function plots a list of (polygon) convex hulls onto a world leaflet map. 
#' 
#' @examples 
#' data <- data.frame(species_name = c("Eucalyptus gunnii", "Eucalyptus tenuiramis", "Eucalyptus globulus"))
#' euc_convex_hulls <- get_range_convex_hulls(data)
#' plot_convex_hulls(convex_hulls = euc_convex_hulls)
#'
#' @export
#' 
plot_convex_hulls <- function(convex_hulls) {

  if (length(convex_hulls) == 0) {
    # Create an empty leaflet map
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
    print(m)
    return(m)  # Return the empty map
  }
  # Extract species names from the names of the list
  species_names <- names(convex_hulls)
  
  # Define a color palette with more flexibility
  n_species <- length(species_names)
  colors <- RColorBrewer::brewer.pal(min(n_species, 12), "Set1")  # Use up to 12 colors from the Set3 palette
  
  if (n_species > 12) {
    # If more than 12 species, generate additional colors
    additional_colors <- colorRampPalette(brewer.pal(12, "Set3"))(n_species - 12)
    colors <- c(colors, additional_colors)
  }
  
  # Create a leaflet map
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
  
  # Initialize a vector to hold legend labels and colors
  legend_labels <- species_names
  legend_colors <- colors[1:n_species]
  
  # Add convex hulls for each species
  for (j in seq_along(convex_hulls)) {
    species_convex_hulls <- convex_hulls[[j]]
    species_name <- species_names[j]
    color <- colors[j]
    
    # Add convex hulls to the map
    for (i in seq_along(species_convex_hulls)) {
      if (!is.null(species_convex_hulls[[i]]) && st_geometry_type(species_convex_hulls[[i]]) == "POLYGON") {
        m <- m %>% addPolygons(
          data = species_convex_hulls[[i]],
          color = color,
          fillOpacity = 0.5,
          weight = 1,
          group = species_name  # Add group for easy legend management
        )
      }
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
