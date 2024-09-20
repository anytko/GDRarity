#' Get Convex Hull Polygons for Species from GBIF Data
#'
#' This function gathers, cleans, and clusters GBIF data to create convex hulls of species ranges. 
#'
#' @param data_frame A dataframe containing scientific names of species. Must include a column named \code{species_name}.
#' @param species_name An optional character vector of species names to process. Default is NULL. If \code{NULL}, all species in the dataframe are processed.
#' @param num_cores The number of cores to use for parallel processing. Default is 1 (no parallel processing).
#' @param min_points The minimum number of points required to form a range cluster. Default is 5.
#' @param min_distance The minimum distance between points in a range cluster. Default is 1 epsilon (eps).
#' @param gbif_limit The maximum number of GBIF records to retrieve per species. Default is 2000.
#' @return A nested list of convex hull polygons for each species. Each species name corresponds to a list of \code{sf} objects representing the convex hulls.
#' @import parallel
#' @import sf
#' @import dplyr
#' @import dbscan
#' @import rgbif
#' @examples
#' # Generate a test dataframe with species Abies cephalonica
#' test_data <- data.frame(species_name = c("Abies_cephalonica"))
#' # Retrieve the unclipped range polygons 
#' unclipped_hulls <- get_range_convex_hulls(test_data)
#' print(unclipped_hulls)
#' @export
#'
get_range_convex_hulls <- function(data_frame, species_name = NULL, num_cores = 1, min_points = 5, min_distance = 1, gbif_limit = 2000) {
  # Validate num_cores
  if (!is.numeric(num_cores) || num_cores <= 0 || floor(num_cores) != num_cores) {
    stop("num_cores must be a positive integer")
  }

  # Validate min_points
  if (!is.numeric(min_points) || min_points <= 0 || floor(min_points) != min_points) {
    stop("min_points must be a positive whole number")
  }

  # Validate min_distance
  if (!is.numeric(min_distance) || min_distance <= 0) {
    stop("min_distance must be a positive number")
  }

  rownames(data_frame) <- gsub("_", " ", rownames(data_frame))

  if (is.null(species_name)) {
    species_names <- data_frame$species_name
  } else {
    species_names <- species_name
  }

  if (num_cores > 1) {
    cluster <- parallel::makeCluster(num_cores)

    # Use all available cores
    species_chunks <- split(species_names, 1:num_cores)

    parallel::clusterEvalQ(cluster, {
      library(dplyr)
      library(sf)
      library(dbscan)
      library(rgbif)
    })
  } else {
    species_chunks <- list(species_names)
  }

  convex_hulls_results <- parallel::mclapply(species_chunks, function(chunk) {
    chunk_result <- list()

    for (species_name in chunk) {
      message("Processing species:", species_name)

      # Remove leading numbers from species name using regular expression
      cleaned_species_name <- gsub("^\\d+\\.", "", species_name)

      gbif_data <- rgbif::occ_search(
        scientificName = cleaned_species_name,
        hasCoordinate = TRUE,
        limit = gbif_limit
      )

      if (length(gbif_data$data) == 0) {
        message(paste("No data found for", cleaned_species_name))
        convex_hulls <- list()
      } else {
        # Check if "occurrenceID" column is present
        if ("occurrenceID" %in% colnames(gbif_data$data)) {
          # Filter based on specified criteria
          gbif_data$data <- gbif_data$data %>%
            filter(
              !is.na(decimalLatitude),
              !is.na(decimalLongitude),
              decimalLatitude >= -90 & decimalLatitude <= 90,
              decimalLongitude >= -180 & decimalLongitude <= 180,
              decimalLatitude != decimalLongitude,
              !duplicated(gbif_data$data$occurrenceID),
              !is.na(countryCode),
              !basisOfRecord %in% c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE", "MATERIAL_CITATION", "FOSSIL_SPECIMEN", "MACHINE_OBSERVATION")
            )
        } else {
          # Handle the case where "occurrenceID" column is not present
          message("The 'occurrenceID' column is not present in the data frame for ", cleaned_species_name)
          convex_hulls <- list()
        }

        if (nrow(gbif_data$data) == 0) {
          message(paste("No data left after filtering for", cleaned_species_name))
          convex_hulls <- list()
        } else {
          gbif_sf <- st_as_sf(gbif_data$data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
          gbif_coords <- st_coordinates(gbif_sf)
          gbif_coords <- as.data.frame(gbif_coords)
          cluster_result <- dbscan(gbif_coords, eps = min_distance, minPts = min_points)
          gbif_coords$Cluster <- cluster_result$cluster
          sdf <- st_as_sf(gbif_coords, coords = c("X", "Y"), crs = 4326)

          sdf_filtered <- sdf[sdf$Cluster != 0, ]

          # Merge overlapping polygons within each cluster
          sdf_list <- lapply(split(sdf_filtered, sdf_filtered$Cluster), function(cluster_data) {
            merged_data <- st_union(cluster_data)
            if (!st_is_valid(merged_data)) {
              merged_data <- st_make_valid(merged_data)
            }
            merged_data
          })

          convex_hulls_list <- lapply(sdf_list, function(merged_data) {
            st_convex_hull(merged_data)
          })

          # Ensure only polygons and multipolygons are kept
          convex_hulls_list <- Filter(function(geom) st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON"), convex_hulls_list)

          chunk_result[[cleaned_species_name]] <- convex_hulls_list
        }
      }
    }

    return(chunk_result)
  }, mc.cores = num_cores)

  if (num_cores > 1) {
    parallel::stopCluster(cluster)
  }

  # Combine the results into a single list
  convex_hulls_results <- do.call(c, convex_hulls_results)

  return(convex_hulls_results)
}

