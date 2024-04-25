#' Calculate Range Size 
#' 
#' Calculate the range size of species within a dataframe using global GBIF occurence data.
#' 
#' @param data_frame A data frame containing species data. Input data frame must include a column named "species_name".
#' 
#' @param species_name A character vector specifying the species names to calculate range sizes. Default is NULL.
#' 
#' @param num_cores Number of CPU cores to use for parallel processing. Default is 1. We recommend utilizing as many cores as possible.
#' 
#' @param min_points The minimum number of points required to form a cluster in DBSCAN. Default is 5.
#' 
#' @param min_distance The maximum distance between points to be considered as neighbors in DBSCAN. Default is 1 unit (1 degree latitude or longitude).
#' 
#' @param gbif_limit The maximum number of occurrences to retrieve from GBIF. Default is 2000. We recommend utilizing a high number of occurences for the best estimate of range size; however, higher gbif_limits will increase computing time. 
#' 
#' @details The function calculates the range size for each species in the provided data frame using data from the Global Biodiversity Information Facility (GBIF). It retrieves occurrence data for each species, creates neighborhood polygons (using dbscan), and calculates the range size (using SF) of the polygons for each species in square kilometers. The function allows for specifying the species for which to calculate range size (via the "species_name" argument) or calculates it for all species in the data frame if not specified.
#' 
#' @return A data frame containing species data with an additional 'range_size' column.
#' 
#' @import rgbif
#' @import sf
#' @import rgbif
#' @import dplyr
#' @import dbscan
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' 
#' Identify species of interest:
#' 
#' species_name <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#'
#' Create dataframe with these species:
#' 
#' tree_data <- data.frame(species_name)
#'
#' Run create_range_size function on this dataframe:
#' 
#' range_size_all <- calc_range_size(data_frame = tree_data, num_cores = 6)
#'
#' print(range_size_all)
#' 
#' Subset the maple species from the dataframe and use calculate_range_size. Adjust the minimum points forming a dbscan neighborhood to 4:
#' 
#' range_size_maple <- calc_range_size(data_frame = tree_data, species_name = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"), num_cores = 6, min_points = 4)
#'
#' print(range_size_maple)
#' 
#' Subset the pine species from the dataframe and use calculate_range_size. Adjust the minimum distance to form a dbscan neighborhood to 1.25 unit latitude/longitude and the maximum gbif occurences to 2500:
#' 
#' range_size_pinus <- calc_range_size(data_frame = tree_data, species_name = c("Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata"), num_cores = 6, min_distance = 1.25, gbif_limit = 2500) 

#' print(range_size_pinus) 
#' 
#' @importFrom Rdpack reprompt
#' 
#' @export 
#' 

calc_range_size <- function(data_frame, species_name = NULL, num_cores = 1, min_points = 5, min_distance = 1, gbif_limit = 2000) {
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

  range_results <- parallel::mclapply(species_chunks, function(chunk) {
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
        range_size <- NA
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
          range_size <- NA
        }

        if (nrow(gbif_data$data) == 0) {
          message(paste("No data left after filtering for", cleaned_species_name))
          range_size <- NA
        } else {
          gbif_sf <- st_as_sf(gbif_data$data, coords = c("decimalLongitude", "decimalLatitude"))
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

          # Calculate areas for all clusters except cluster 0
          areas <- sapply(convex_hulls_list, function(ch) {
            st_area(ch) / 1e6  # Convert to square kilometers
          })

          # Calculate range size for species
          if (length(areas) == 0) {
            range_size <- 0  # Set to a non-zero value for species with one cluster
          } else {
            range_size <- sum(areas)
          }
        }

        chunk_result[[cleaned_species_name]] <- range_size
      }
    }

    return(chunk_result)
  }, mc.cores = num_cores)

  if (num_cores > 1) {
    parallel::stopCluster(cluster)
  }

  # Combine the results into a single list
  range_results <- do.call(c, range_results)

  # Convert the list to a data frame
  result_df <- data.frame(species_name = names(range_results), range_size = unlist(range_results), row.names = NULL)

  rownames(result_df) <- NULL

  return(result_df)
}




