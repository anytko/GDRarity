#' Clip polygons to land boundaries
#'
#' This function takes a list of polygons and clips them to land boundaries defined by a continent sf object.
#'
#' @param polygons_list A list of polygons to be clipped.
#' @param continent_sf An sf object containing polygons representing continents. 
#' @return A list of clipped polygons, each element corresponding to a polygon in the input list.
#' @import sf
#' @import rmapshaper
#' @export

clip_polygons_to_land <- function(polygons_list, continent_sf) {
  # Check if the input is NULL
  if (is.null(polygons_list) || length(polygons_list) == 0) {
    return(NULL)
  }
  
  clipped_polygons_list <- lapply(polygons_list, function(convex_hull) {
    # Convert convex_hull to an sf object if not already
    if (!inherits(convex_hull, "sf")) {
      convex_hull <- st_as_sf(convex_hull)
    }
    
    # Perform the clipping operation
    land_polygons <- ms_clip(convex_hull, continent_sf)
    
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
  })
  
  # Filter out NULL results
  clipped_polygons_list <- Filter(Negate(is.null), clipped_polygons_list)
  
  return(clipped_polygons_list)
}