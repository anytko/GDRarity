#' Compute range sizes (sq km) from clipped or raw range polygons
#'
#' Calculates the total range size (in square kilometers) for each species 
#' from a list of range polygons. Can be used with polygons that have been 
#' clipped to land boundaries or with raw convex hull polygons.
#'
#' @param clipped_polygons_list A named list where each element is a list of 
#'   `sf` polygon objects representing the range of a species.
#' @param species_col Character string specifying the name of the species column 
#'   in the output. Default is `"species_name"`.
#' 
#' @details
#' For each species, the function sums the areas of all polygons in 
#' `clipped_polygons_list` using `sf::st_area()`. Areas are returned in square 
#' kilometers by dividing the default square meters result by 1e6.
#' 
#' Polygons that are invalid (`!st_is_valid()`) are skipped, and any errors 
#' during area calculation are caught and reported as warnings.
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item `species_col` — species name.
#'     \item `range_size` — total range size in square kilometers.
#'   }
#' 
#' @import sf
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang sym
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
range_sizes <- function(clipped_polygons_list, species_col = "species_name") {
  if (is.null(clipped_polygons_list) || length(clipped_polygons_list) == 0) {
    return(tibble::tibble(!!sym(species_col) := character(), range_size = numeric()))
  }
  
  species_range_sizes <- lapply(names(clipped_polygons_list), function(sp_name) {
    total_area <- 0
    clipped_polygons <- clipped_polygons_list[[sp_name]]
    
    for (clipped_polygon in clipped_polygons) {
      tryCatch({
        if (sf::st_is_valid(clipped_polygon)) {
          area <- sf::st_area(clipped_polygon) / 1e6  # km^2
          total_area <- total_area + as.numeric(area)
        }
      }, error = function(e) {
        warning("Error calculating area for clipped polygon: ", conditionMessage(e))
      })
    }
    
    tibble::tibble(
      !!sym(species_col) := sp_name,
      range_size = total_area
    )
  })
  
  result_df <- dplyr::bind_rows(species_range_sizes)
  return(result_df)
}

