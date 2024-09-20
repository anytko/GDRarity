#' Calculate range sizes for multiple species 
#'
#' This function calculates the range sizes (sq km) of multiple species by retrieving occurrence data from GBIF, generating convex hulls, and clipping them to continent boundaries. It uses parallel processing to handle multiple species efficiently.
#'
#' @param data_frame A \code{data.frame} containing scienfic names of species with a column named \code{species_name}.
#' @param num_cores The number of cores to use for parallel processing. Default is 1 (no parallel processing).
#' @param min_points The minimum number of points required to form a range cluster. Default is 5.
#' @param min_distance The minimum distance between points in a range cluster. Default is 1 epsilon (eps).
#' @param gbif_limit The maximum number of GBIF records to retrieve per species. Default is 2000.
#' @param continent_file The URL of a GeoJSON or shape (.shp & .shx) file containing country or continent data. Default is NULL - If \code{NULL}, a GeoJSON file from Natural Earth will be used.
#' @return A \code{data.frame} with two columns: \code{species_name} and \code{range_size}. The \code{range_size} column represents the size of the range for each species in square kilometers.
#' @import future
#' @import future.apply
#' @import dplyr
#' @import sf
#' @import parallel
#' @import utils
#' @examples
#' # Generate a test dataframe with species Abies cephalonica
#' test_data <- data.frame(species_name = c("Betula_humilis"))
#' 
#' # Generate range sizes for each speices
#' range_size <- calc_range_size(data_frame = test_data, min_points = 4)
#' print(range_size)
#' @export
#' 
calc_range_size <- function(data_frame, num_cores = 1, min_points = 5, min_distance = 1, gbif_limit = 2000, continent_file = NULL) {
  # Define a function to calculate the range sizes for a given species
  calculate_species_range_size <- function(species_name) {
    # Call get_range_polygons to get the convex hulls for the species
    convex_hulls <- get_range_convex_hulls(data_frame, species_name = species_name, num_cores = 1, 
                                       min_points = min_points, min_distance = min_distance, gbif_limit = gbif_limit)
    
    # Use the provided continent file or default to the Natural Earth continent boundaries
    if (is.null(continent_file)) {
      continent_sf <- get_continent_sf()
    } else {
      continent_sf <- st_read(continent_file)  # Load the user-provided continent file
    }
    
    # Clip the convex hulls to the continent boundaries
    clipped_polygons_list <- clip_polygons_to_land(convex_hulls, continent_sf)
    
    # Calculate the range sizes of the clipped polygons and return a data frame
    range_size_df <- range_sizes(clipped_polygons_list)
    
    return(range_size_df)
  }
  
  # Get unique species names from the data frame
  species_names <- unique(data_frame$species_name)
  
  # Set up future plan for parallel processing
  plan(multicore, workers = num_cores)
  
  # Use future_lapply for parallel processing with future.seed = TRUE
  range_size_results <- future_lapply(species_names, calculate_species_range_size, future.seed = TRUE)
  
  # Combine the resulting data frames into one data frame
  result_df <- do.call(rbind, range_size_results)
  
  return(result_df)
}

