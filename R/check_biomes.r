#' Analyze Species Convex Hulls Against Biome Data
#'
#' Checks which biomes are intersected by the convex hulls of species distributions.
#' By default, uses a preloaded biome shapefile containing four biomes: 
#' Humid Tropics, Dry Tropics, Temperate, and Boreal. 
#' Users can optionally supply their own biome shapefile as an `sf` object.
#'
#' @param convex_hulls A named list of convex hulls for each species. 
#'   Each element should be a list of `sf` polygon objects representing the convex hulls of a species' distribution.
#' 
#' @param biome_sf Optional `sf` object representing biome polygons. 
#'   If not provided, the default biome shapefile (`biomes.shp`) included in the package will be used. Custom biome data must include a column named `"BIOME"`.
#'
#' @details
#' The function ensures that both the biome data and species convex hulls 
#' are in the EPSG:4326 (WGS84) coordinate reference system. If CRS is missing, 
#' it is assigned automatically. Polygons are validated with `st_make_valid()` before intersection checks. 
#' Returns a dataframe where each row corresponds to a species, and each biome column contains `1` if an intersection occurs, otherwise `0`.
#'
#' @return A dataframe with `species_name` and one column per biome. 
#'   Values are `1` if the species' convex hull intersects the biome, `0` otherwise.
#'
#' @note Custom biome data must have a `"BIOME"` column with biome names.
#'
#' @examples 
#' # Example: Checking biome intersections for two Abies species
#' test_data <- data.frame(species_name = c("Abies_cephalonica", "Abies_firma"))
#' convex_hulls_abies <- get_range_convex_hulls(test_data)
#' abies_biomes <- check_biomes(convex_hulls_abies)
#' print(abies_biomes)
#'
#' @references
#' \insertRef{Hansen2010}{GDRarity}
#'
#' @import sf
#' @importFrom dplyr bind_rows
#' @export
check_biomes <- function(convex_hulls, biome_sf = NULL) {
  # Path to the biome shapefile within the extdata directory
  shapefile_path <- system.file("extdata", "biomes.shp", package = "GDRarity")
  
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

