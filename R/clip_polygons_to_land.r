#' Clip species range polygons to land boundaries
#'
#' This function uses a continent GeoJSON or shape file (.shp & .shx) file to clip range polygons to land boundaries.
#'
#' @param convex_hulls A nested list of `sf` polygon objects representing the convex hulls 
#'   of species ranges. Typically generated using [get_range_convex_hulls()].
#' @param continent_sf An `sf` object containing continent polygons (e.g., from Natural Earth 
#'   GeoJSON or shapefiles). Coordinate reference systems will be matched automatically
#' @return A nested list of `sf` polygon objects representing the clipped species ranges. 
#'   Each top-level list element corresponds to a species; each sublist contains one or 
#'   more clipped polygons for that species.
#' @import rmapshaper
#' @import sf
#' @details
#' The function sets the CRS of each input polygon to match that of `continent_sf` using 
#' `sf::st_crs()`, then uses `rmapshaper::ms_clip()` to perform the clipping operation. 
#' Invalid geometries are fixed with `sf::st_make_valid()`.
#' If a species range does not intersect land (i.e., an entirely oceanic polygon), it is 
#' removed from the output. Errors during clipping are caught and silently ignored.
#' 
#' @examples
#' # Generate a test dataframe with species Abies cephalonica
#' test_data <- data.frame(species_name = c("Acer_campestre"))
#' # Retrieve the unclipped range convex hulls 
#' unclipped_hulls <- get_range_convex_hulls(test_data)
#' # Get continent data from a GeoJSON file
#' continent_sf_example <- get_continent_sf()
#' # Clip the range polygons to continent bounds 
#' clipped_polygons <- clip_polygons_to_land(convex_hulls = unclipped_hulls, continent_sf = continent_sf_example)
#' print(clipped_polygons)
#' @export
clip_polygons_to_land <- function(convex_hulls, continent_sf) {
  # Check if the input is NULL
  if (is.null(convex_hulls) || length(convex_hulls) == 0) {
    return(NULL)
  }
  
  clipped_polygons_list <- lapply(convex_hulls, function(convex_hulls) {
    clipped_polygons <- lapply(convex_hulls, function(ch) {
      result <- tryCatch({
        # Set the CRS of the input polygon to match the CRS of the continent polygons
        st_crs(ch) <- st_crs(continent_sf)
        
        # Perform the clipping operation
        land_polygons <- ms_clip(ch, continent_sf)
        
        # Ensure validity and clean the geometry
        if (!is.null(land_polygons) && length(land_polygons) > 0) {
          valid_polygons <- st_make_valid(land_polygons)
          if (st_is_empty(valid_polygons)) {
            return(NULL)
          } else {
            return(valid_polygons)
          }
        } else {
          return(NULL)
        }
      }, error = function(e) {
        # Catch the error but do not print it
        return(NULL)
      })
      
      return(result)
    })
    # Filter out NULL results
    clipped_polygons <- Filter(Negate(is.null), clipped_polygons)
    return(clipped_polygons)
  })
  
  return(clipped_polygons_list)
}
