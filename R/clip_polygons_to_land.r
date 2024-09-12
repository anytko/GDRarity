#' Clip species range polygons to land boundaries
#'
#' This function uses a continent GeoJSON or shape file (.shp & .shx) file to clip range polygons to land boundaries.
#'
#' @param polygons_list A nested list of \code{sf} objects representing the convex hulls of a species range.
#' @param continent_sf A file of continent boundaries. We recommend using Natural Earth's GeoJSON or shape files. 
#' @return A nested list of clipped range polygons
#' @import rmapshaper
#' @import sf
#' @details When clipping the polygons of species with no points touching land (i.e. an oceanic polygon) the function will return an error. This error has been silenced and the oceanic polygon will be removed from the clipped polygon list.
#' 
#' @examples
#' Generate a test dataframe with species Abies cephalonica
#' test_data <- data.frame(species_name = c("Abies_cephalonica"))
#' Retrieve the unclipped range convex hulls 
#' unclipped_hulls <- get_range_convex_hulls(test_data)
#' # Get continent data from a GeoJSON file
#' continent_sf_example <- get_continent_sf("https://d2ad6b4ur7yvpq.cloudfront.net/naturalearth-3.3.0/ne_50m_admin_0_countries.geojson")
#' # Clip the range polygons to continent bounds 
#' clipped_polygons <- clip_polygons_to_land(polygons_list = unclipped_hulls, continent_sf = continent_sf_example)
#' print(clipped_polygons)
#' @export
#' 
clip_polygons_to_land <- function(polygons_list, continent_sf) {
  # Check if the input is NULL
  if (is.null(polygons_list) || length(polygons_list) == 0) {
    return(NULL)
  }
  
  clipped_polygons_list <- lapply(polygons_list, function(convex_hulls) {
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
