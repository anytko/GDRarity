#' Get Convex Hull Polygons for Species from GBIF Data
#'
#' Retrieves occurrence data for species from GBIF, clusters spatial points using DBSCAN, and generates convex hull polygons for each cluster. 
#'
#' @param data_frame A dataframe containing scientific names of species. Must include a column named \code{species_name}.
#' @param species_col Character string specifying the column in `data_frame` that contains species names. Default is `"species_name"`.
#' @param species_name An optional character vector of species names to process. Default is NULL. If \code{NULL}, all species in the dataframe are processed.
#' @param num_cores The number of cores to use for parallel processing. Default is 1 (no parallel processing).
#' @param min_points The minimum number of points required to form a range cluster. Default is 5.
#' @param min_distance The minimum distance between points in a range cluster. Default is 1 epsilon (eps).
#' @param gbif_limit The maximum number of GBIF records to retrieve per species. Default is 2000.
#' 
#' @details
#' The function:
#' 1. Retrieves occurrence records from GBIF using [rgbif::occ_search()].
#' 2. Filters and cleans records (removing invalid coordinates, duplicates, and certain basis of record types).
#' 3. Clusters spatial points using [dbscan::dbscan()] with `eps = min_distance` and `minPts = min_points`.
#' 4. Computes convex hull polygons for each cluster using `sf::st_convex_hull()`.
#'
#' Parallelization is handled via [parallel::mclapply()] when `num_cores > 1`.
#' 
#' @return A named list where each element corresponds to a species and contains a list of `sf` polygon objects representing convex hulls.
#' 
#' @import parallel
#' @import sf
#' @import dplyr
#' @import dbscan
#' @import rgbif
#' @importFrom magrittr %>%
#' 
#' @examples
#' # Generate a test dataframe with species Abies cephalonica
#' \dontrun{
#' test_data <- data.frame(species_name = c("Abies_cephalonica"))
#' # Retrieve the unclipped range polygons 
#' unclipped_hulls <- get_range_convex_hulls(test_data)
#' print(unclipped_hulls)
#' }
#' @export
get_range_convex_hulls <- function(data_frame, species_col = "species_name", species_name = NULL,
                                      num_cores = 1, min_points = 5, min_distance = 1, gbif_limit = 2000) {
  # Validate inputs
  if (!is.numeric(num_cores) || num_cores <= 0 || floor(num_cores) != num_cores) {
    stop("num_cores must be a positive integer")
  }
  if (!is.numeric(min_points) || min_points <= 0 || floor(min_points) != min_points) {
    stop("min_points must be a positive whole number")
  }
  if (!is.numeric(min_distance) || min_distance <= 0) {
    stop("min_distance must be a positive number")
  }
  
  # Check that species_col exists in data_frame
  if (!(species_col %in% colnames(data_frame))) {
    stop(paste("Species column", species_col, "not found in data frame"))
  }
  
  rownames(data_frame) <- gsub("_", " ", rownames(data_frame))
  
  # Determine species names to process:
  if (!is.null(species_name)) {
    # Use the provided species_name vector directly
    species_names <- species_name
  } else {
    # Extract unique species names from specified column
    species_names <- unique(data_frame[[species_col]])
  }
  
  # Split species evenly across cores
  if (length(species_names) == 1 || num_cores == 1) {
    species_chunks <- list(species_names)
  } else {
    species_chunks <- split(
      species_names,
      cut(seq_along(species_names), breaks = min(num_cores, length(species_names)), labels = FALSE)
    )
  }
  
  # Parallel processing
  convex_hulls_results <- parallel::mclapply(species_chunks, function(chunk) {
    chunk_result <- list()
    
    for (sp_name in chunk) {
      message("Processing species: ", sp_name)
      
      cleaned_species_name <- gsub("^\\d+\\.", "", sp_name)
      
      # Attempt GBIF search with error handling
      gbif_data <- tryCatch({
        rgbif::occ_search(
          scientificName = cleaned_species_name,
          hasCoordinate = TRUE,
          limit = gbif_limit
        )
      }, error = function(e) {
        message("GBIF error for ", cleaned_species_name, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(gbif_data) || is.null(gbif_data$data) || nrow(gbif_data$data) == 0) {
        message(paste("No data found for", cleaned_species_name))
        next
      }
      
      # Clean GBIF data
      if ("occurrenceID" %in% colnames(gbif_data$data)) {
        gbif_data$data <- gbif_data$data %>%
          dplyr::filter(
            !is.na(decimalLatitude),
            !is.na(decimalLongitude),
            decimalLatitude >= -90 & decimalLatitude <= 90,
            decimalLongitude >= -180 & decimalLongitude <= 180,
            decimalLatitude != decimalLongitude,
            !duplicated(occurrenceID),
            !is.na(countryCode),
            !basisOfRecord %in% c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE", "MATERIAL_CITATION", 
                                  "FOSSIL_SPECIMEN", "MACHINE_OBSERVATION")
          )
      } else {
        message("No 'occurrenceID' column for ", cleaned_species_name)
        next
      }
      
      if (nrow(gbif_data$data) == 0) {
        message(paste("No data left after filtering for", cleaned_species_name))
        next
      }
      
      # Convert to sf and run clustering
      gbif_sf <- sf::st_as_sf(gbif_data$data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
      gbif_coords <- sf::st_coordinates(gbif_sf) %>% as.data.frame()
      dbscan_result <- dbscan::dbscan(gbif_coords, eps = min_distance, minPts = min_points)
      
      gbif_coords$Cluster <- dbscan_result$cluster
      sdf <- sf::st_as_sf(gbif_coords, coords = c("X", "Y"), crs = 4326)
      sdf_filtered <- sdf[sdf$Cluster != 0, ]
      
      if (nrow(sdf_filtered) == 0) {
        message(paste("No clusters found for", cleaned_species_name))
        next
      }
      
      # Compute convex hulls
      sdf_list <- lapply(split(sdf_filtered, sdf_filtered$Cluster), function(cluster_data) {
        merged_data <- sf::st_union(cluster_data)
        if (!sf::st_is_valid(merged_data)) {
          merged_data <- sf::st_make_valid(merged_data)
        }
        merged_data
      })
      
      convex_hulls_list <- lapply(sdf_list, function(merged_data) {
        sf::st_convex_hull(merged_data)
      })
      
      # Keep only polygon geometries
      convex_hulls_list <- Filter(function(geom) {
        sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")
      }, convex_hulls_list)
      
      if (length(convex_hulls_list) > 0) {
        chunk_result[[cleaned_species_name]] <- convex_hulls_list
      } else {
        message(paste("No valid convex hulls for", cleaned_species_name))
      }
    }
    
    return(chunk_result)
  }, mc.cores = num_cores)
  
  # Flatten results and drop NULLs
  flat_results <- do.call(c, convex_hulls_results)
  flat_results <- flat_results[!sapply(flat_results, is.null)]
  
  return(flat_results)
}
