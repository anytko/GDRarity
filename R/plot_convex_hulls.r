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
#' data <- data.frame(species_name = c("Quercus alba"))
#' oak_convex_hulls <- create_range_polygons(data)
#' plot_convex_hulls(convex_hulls = oak_convex_hulls)
#'
#' @export

plot_convex_hulls <- function(convex_hulls) {
  # Create a leaflet map
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to world
  
  # Add convex hulls for each species
  for (species_convex_hulls in convex_hulls) {
    for (i in 1:length(species_convex_hulls)) {
      if (!is.null(species_convex_hulls[[i]]) && st_geometry_type(species_convex_hulls[[i]]) == "POLYGON") {
        m <- m %>% addPolygons(data = species_convex_hulls[[i]], color = "blue")
      }
    }
  }
  
  # Print the map
  print(m)
}

