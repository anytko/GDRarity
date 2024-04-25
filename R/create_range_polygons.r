#' Create range polygons
#' 
#' @param data_frame A data frame containing species data.
#' @param species_name A character vector of species names.
#' @param min_points Minimum number of points to form a cluster using dbscan.
#' @param min_distance Minimum distance between points in the same cluster.
#' @param gbif_limit Maximum number of records to fetch from GBIF.
#' @return A list of convex hulls for each species, or NULL if no data is found.
#' @details The function creates convex hulls for species in a dataframe using gbif occurence data. Convex hulls will vary based on the minimum points and distance between points used to define a polygon. 
#' 
#' @import rgbif
#' @import sf
#' @import dbscan
#' 
#' @examples 
#' data <- data.frame(species_name = c("Quercus alba"))
#' convex_hulls <- create_range_polygons(data)
#'
#' print(convex_hulls)
#'
#' @export

create_range_polygons <- function(data_frame, species_name = NULL, min_points = 5, min_distance = 1, gbif_limit = 2000) {
  rownames(data_frame) <- gsub("_", " ", rownames(data_frame))
  
  if (is.null(species_name)) {
    species_names <- data_frame$species_name
  } else {
    species_names <- species_name
  }
  
  range_results <- lapply(species_names, function(species_name) {
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
      return(NA)
    }
    
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
      return(NA)
    }
    
    if (nrow(gbif_data$data) == 0) {
      message(paste("No data left after filtering for", cleaned_species_name))
      return(NA)
    }
    
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
    
    # Calculate convex hulls
    convex_hulls <- lapply(sdf_list, function(merged_data) {
      st_convex_hull(merged_data)
    })
    
    return(convex_hulls)
  })
  
  return(range_results)
}


