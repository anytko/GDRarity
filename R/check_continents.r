#' Check Range Intersections with Continent Polygons
#'
#' This function checks whether the polygons representing species ranges intersect with polygons representing continents. It returns a dataframe indicating which species ranges intersect with which continents.
#'
#' @param convex_hulls A nested list of convex hulls of species ranges. Can be generated using GeoFunPhy::get_range_convex_hulls() or clip_polygons_to_land().
#' @param continent_sf An `sf` object containing polygons representing continents. Can be generated using GeoFunPhy::get_continent_sf()
#'
#' @details
#' The function assumes that both the species range polygons and continent polygons are in the same coordinate reference system. If the range or continent polygons are invalid, they are automatically made valid using `st_make_valid`. The function returns a dataframe where each row corresponds to a species, and each column corresponds to a continent. A value of `1` indicates that the species range intersects with the continent polygon, while `0` indicates no intersection.
#'
#' @return A dataframe with species names as rows and continent names as columns. Each cell contains a `1` if the corresponding species range intersects with the continent polygon, and `0` otherwise.
#'
#' @import sf
#' @import dplyr
#'
#' @examples 
#' # Generate a dataframe for species Abies cephalonica and Abies firma
#' test_data <- data.frame(species_name = c("Abies_cephalonica", "Abies_firma"))
#' 
#' # Get continent bounds
#' continent_bounds <- get_continent_sf()
#' 
#' # Find convex hulls for Abies cephalonica and Abies firma
#' test_hulls <- GeoFunPhy::get_range_convex_hulls(test_data)
#' 
#' # Clip convex hulls to continent bounds
#' clipped_polys <- clip_polygons_to_land(test_hulls, continent_bounds)
#' 
#' # Check intersections with continents - Abies cephalonica is found in Europe and Abies firma is found in Asia
#' intersections <- check_continents(convex_hulls = clipped_polys, continent_sf = continent_bounds)
#' print(intersections)
#' 
#' @export
#' 
check_continents <- function(convex_hulls, continent_sf) {
  # Check if continent_sf is missing
  if (missing(continent_sf)) {
    stop("Error: The continent_sf input is missing. Please provide a valid sf object with geometries.")
  }
  
  # Check if continent_sf is an sf object
  if (!inherits(continent_sf, "sf")) {
    stop("Error: The continent_sf input is not an sf object. Please provide a valid sf object with geometries.")
  }
  
  # Ensure continent polygons are valid
  continent_sf <- tryCatch(
    {
      # Check if continent_sf has valid geometries
      if (any(!st_is_valid(continent_sf))) {
        st_make_valid(continent_sf)
      } else {
        continent_sf
      }
    },
    error = function(e) {
      stop("Error: The continent_sf input contains invalid geometries. Please provide valid geometries.")
    }
  )
  
  # Check if convex_hulls is empty and return an error if it is
  if (length(convex_hulls) == 0) {
    stop("Error: The convex_hulls list is empty. Please provide valid convex hulls.")
  }
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Iterate over each species in the range list
  for (species_name in names(convex_hulls)) {
    species_data <- convex_hulls[[species_name]]
    
    # Initialize an empty dataframe for the current species
    species_df <- data.frame(matrix(0, nrow = 1, ncol = nrow(continent_sf)))
    colnames(species_df) <- continent_sf$continent
    
    # Iterate over each polygon within the species
    for (polygon_index in seq_along(species_data)) {
      polygon <- species_data[[polygon_index]]
      
      # Ensure polygon is valid
      polygon <- st_make_valid(polygon)
      
      # Run st_intersects for each polygon
      intersection_result <- st_intersects(polygon, continent_sf, sparse = FALSE)
      
      # Update the species_df dataframe based on intersection results
      if (any(unlist(intersection_result))) {
        intersected_indices <- which(unlist(intersection_result))
        species_df[1, intersected_indices] <- 1
      }
    }
    
    # Add species name as a column
    species_df$species_name <- species_name
    
    # Append species dataframe to results list
    results_list[[species_name]] <- species_df
  }
  
  # Combine all species dataframes into a single dataframe
  results_df <- bind_rows(results_list)
  
  return(results_df)
}




