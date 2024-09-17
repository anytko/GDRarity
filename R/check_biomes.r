#' Analyze Species Convex Hulls Against Biome Data
#'
#' This function checks which biomes are intersected by the convex hulls of species distributions. By default, the function uses a preloaded biome shapefile containing four biomes: Humid Tropics, Dry Tropics, Temperate, and Boreal. Users can also provide their own biome data as an `sf` object.
#'
#' @param convex_hulls A named list of convex hulls for each species. Each element of the list should contain a list of `sf` polygon objects representing the convex hulls of species' distributions.
#' @param biome_sf An optional `sf` object representing the biome polygons. If not provided, the default biome shapefile (`biomes.shp`) from the package will be used. The default biomes are:
#' \itemize{
#'   \item \strong{Humid Tropics}
#'   \item \strong{Dry Tropics}
#'   \item \strong{Temperate}
#'   \item \strong{Boreal}
#' }
#'
#' @return A dataframe where each row corresponds to a species, and each column (except `species_name`) represents a biome. A value of 1 in the cell indicates that the species' convex hull intersects with the respective biome.
#'
#' @note If the CRS of the provided `biome_sf` or `convex_hulls` is not set, the function will automatically assign and transform them to EPSG:4326 (WGS84).
#' 
#' @examples 
#' 
#' # Generate a dataframe for Abies cephalonica and Abies firma
#' test_data <- data.frame(species_name = c("Abies_cephalonica", "Abies_firma"))
#' 
#' # Get convex hulls for species' ranges 
#' convex_hulls_abies <- get_range_convex_hulls(test_data)
#' 
#' # Check the biomes of Abies cephalonica and Abies firma - Abies cephalonica occurs in temperate forests while Abies firma occures in both temperate forests and humid tropics
#' abies_biomes <- check_biomes(convex_hulls_abies)
#' 
#' #' @references
#' \insertRef{Hansen2010}{GeoFunPhy}
#' 
#' @export
#'
check_biomes <- function(convex_hulls, biome_sf = NULL) {
  # Path to the biome shapefile within the extdata directory
  shapefile_path <- system.file("extdata", "biomes.shp", package = "GeoFunPhy")
  
  # Check if the user has provided their own biome data
  if (is.null(biome_sf)) {
    # If not provided, read the biome shapefile from the package
    if (shapefile_path == "") {
      stop("Shapefile not found. Ensure the shapefile is in the 'extdata' directory of the package.")
    }

    # Read the biome shapefile
    biome_sf <- st_read(shapefile_path)
  } else {
    # Ensure provided biome_sf is a valid sf object
    if (!inherits(biome_sf, "sf")) {
      stop("Provided biome_sf must be an sf object.")
    }
  }

  # Set CRS for biome_sf if not already set
  if (is.na(st_crs(biome_sf))) {
    st_crs(biome_sf) <- 4326
  }

  target_crs <- st_crs(4326)

  # Transform biome_sf to target CRS
  biome_sf <- st_transform(biome_sf, crs = target_crs)
  
  # Ensure biome polygons are valid
  biome_sf <- st_make_valid(biome_sf)
  
  # Transform convex_hulls to the target CRS
  convex_hulls <- lapply(convex_hulls, function(hulls) {
    lapply(hulls, function(hull) {
      st_transform(hull, crs = target_crs)
    })
  })
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Iterate over each species in the range list
  for (species_name in names(convex_hulls)) {
    species_data <- convex_hulls[[species_name]]
    
    # Initialize an empty dataframe for the current species
    species_df <- data.frame(matrix(0, nrow = 1, ncol = nrow(biome_sf)))
    colnames(species_df) <- biome_sf$BIOME  # Use BIOME column for column names
    
    # Iterate over each polygon within the species
    for (polygon_index in seq_along(species_data)) {
      polygon <- species_data[[polygon_index]]
      
      # Ensure polygon is valid
      polygon <- st_make_valid(polygon)
      
      # Run st_intersects for each polygon
      intersection_result <- st_intersects(polygon, biome_sf, sparse = FALSE)
      
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

