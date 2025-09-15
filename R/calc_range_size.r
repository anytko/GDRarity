#' Calculate range sizes for multiple species 
#'
#' This function calculates the range sizes (sq km) of multiple species by retrieving occurrence data from GBIF, generating convex hulls, and clipping them to continent boundaries. It uses parallel processing to handle multiple species efficiently.
#'
#' @param data_frame A \code{data.frame} containing scienfic names of species with a column named \code{species_name}.
#' @param species_col A character string indicating the column in \code{data_frame} that contains species names. Default is \code{"species_name"}.
#' @param num_cores The number of cores to use for parallel processing. Default is 1 (no parallel processing).
#' @param min_points The minimum number of points required to form a range cluster. Default is 5.
#' @param min_distance The minimum distance between points in a range cluster. Default is 1 epsilon (eps).
#' @param gbif_limit The maximum number of GBIF records to retrieve per species. Default is 2000.
#' @param continent_file Optional path or URL to a GeoJSON or shapefile 
#'   (`.shp` and `.shx`) containing continent or country boundaries. 
#'   If `NULL` (default), Natural Earth continent data is retrieved via 
#'   [get_continent_sf()].
#' 
#' @details 
#' This function wraps the following internal steps:
#' 1. Retrieves GBIF occurrence data via [get_range_convex_hulls()].
#' 2. Reads continent boundaries via [get_continent_sf()] or from a user-supplied file.
#' 3. Clips convex hull polygons to land boundaries using [clip_polygons_to_land()].
#' 4. Calculates total polygon area (km²) per species using [range_sizes()].
#'
#' Parallelization is handled via **future** and **future.apply**. 
#' Each species is processed independently and requires internet accesss.

#' @return A `data.frame` with two columns:
#'   \itemize{
#'     \item `species_col` — species name.
#'     \item `range_size` — total range size in square kilometers.
#'   }
#' @import future
#' @import future.apply
#' @import dplyr
#' @import sf
#' @import parallel
#' @import utils
#' @import rmapshaper
#' @examples
#' # Generate a test dataframe with species Abies cephalonica
#' \dontrun{
#' test_data <- data.frame(species = c("Abies cephalonica"))
#' 
#' # Generate range sizes for each speices
#' range_size <- calc_range_size(data_frame = test_data, min_points = 4, species_col = "species")
#' print(range_size)
#' }
#' @export
#' 
calc_range_size <- function(data_frame, species_col = "species_name", num_cores = 1, 
                            min_points = 5, min_distance = 1, gbif_limit = 2000, continent_file = NULL) {
  
  calculate_species_range_size <- function(species_name) {
    convex_hulls <- get_range_convex_hulls(data_frame, species_col = species_col,
                                               species_name = species_name,  
                                               num_cores = 1, min_points = min_points, 
                                               min_distance = min_distance, gbif_limit = gbif_limit)
    
    if (is.null(continent_file)) {
      continent_sf <- get_continent_sf()
    } else {
      continent_sf <- sf::st_read(continent_file)
    }
    
    clipped_polygons_list <- clip_polygons_to_land(convex_hulls, continent_sf)
    range_size_df <- range_sizes(clipped_polygons_list, species_col)
    return(range_size_df)
  }
  
  if (!(species_col %in% colnames(data_frame))) {
    stop(paste("Species column", species_col, "not found in data frame"))
  }
  
  species_names <- unique(data_frame[[species_col]])
  
  plan(future::multicore, workers = num_cores)
  range_size_results <- future.apply::future_lapply(species_names, calculate_species_range_size, future.seed = TRUE)
  
  result_df <- do.call(rbind, range_size_results)
  return(result_df)
}
