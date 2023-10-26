#' Calculate Range Size 
#' 
#' Calculate the range size of species within a dataframe using global GBIF occurence data.
#' 
#' @param data_frame A data frame containing species data. Input data frame must include species names as rownames.
#' 
#' @param species_name A character vector specifying the species names to calculate range sizes. Default is NULL.
#' 
#' @param resolution Resolution for raster conversion. Default resolution is 0.1.
#' 
#' @details The function calculates the range size for each species in the provided data frame using data from the Global Biodiversity Information Facility (GBIF). It retrieves occurrences data for each species, creates a raster representation, and calculates the range size. The function allows for specifying the species for which to calculate range size (via the "species_name" argument) or calculates it for all species in the data frame if not specified.
#' 
#' @return A data frame containing species data with an additional 'range_size' column.
#' 
#' @import raster
#' @import rgbif
#' @import sp
#' @import rlist
#' @import tidyr
#' @import terra
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' range_size_all <- calculate_range_size(data_frame = tree_data)
#' 
#' range_size_maple <- calculate_range_size(data_frame = tree_data, species_name = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"))
#' 
#' range_size_populus_res <- calculate_range_size(data_frame = tree_data, species_name = c("Populus_alba", "Populus_candicans", "Populus_nigra", "Populus_tremula", "Populus_tremuloides", "Populus_trichocarpa"), resolution = 0.5) 
#' 
#' @export 
#' 
calculate_range_size <- function(data_frame, species_name = NULL, resolution = 0.1) {
  
  rownames(data_frame) <- gsub("_", " ", rownames(data_frame))

  if (is.null(species_name)) {
    
    species_names <- rownames(data_frame)
  } else {
    
    species_names <- species_name
  }

 
  range_results <- list()

  for (species_name in species_names) {
    gbif_data <- rgbif::occ_search(
      scientificName = species_name,
      hasCoordinate = TRUE,
      limit = 100
    )

    if (length(gbif_data$data) == 0) {
      message(paste("No data found for", species_name))
      
      range_size <- NA
    } else {
      
      coordinates <- cbind(gbif_data$data$decimalLongitude, gbif_data$data$decimalLatitude)
      gbif_points <- SpatialPointsDataFrame(coordinates, gbif_data$data)

      proj4string(gbif_points) <- CRS("+proj=longlat +datum=WGS84")

      
      gbif_vector <- vect(gbif_points)

      
      gbif_raster <- rast(gbif_vector, res = resolution)

      
      cropped_rasters <- list()

      euro_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(-25, 70, 36, 70)), error = function(e) NULL)
      na_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(-168, -35, 7, 83)), error = function(e) NULL)
      oc_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(113, 153, -55, -10)), error = function(e) NULL)
      africa_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(-25, 50, -35, 37)), error = function(e) NULL)
      asia_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(25, 180, -12, 77)), error = function(e) NULL)
      sa_cropped_range <- tryCatch(terra::crop(gbif_raster, ext(-79, -34, -56, 12)), error = function(e) NULL)

    
     euro_range_size <- if (!is.null(euro_cropped_range)) sum(terra::expanse(euro_cropped_range, unit = "km")) else 0
    na_range_size <- if (!is.null(na_cropped_range)) sum(terra::expanse(na_cropped_range, unit = "km")) else 0
    oc_range_size <- if (!is.null(oc_cropped_range)) sum(terra::expanse(oc_cropped_range, unit = "km")) else 0
    africa_range_size <- if (!is.null(africa_cropped_range)) sum(terra::expanse(africa_cropped_range, unit = "km")) else 0
    asia_range_size <- if (!is.null(asia_cropped_range)) sum(terra::expanse(asia_cropped_range, unit = "km")) else 0
    sa_range_size <- if (!is.null(sa_cropped_range)) sum(terra::expanse(sa_cropped_range, unit = "km")) else 0

      
      range_size <- sum(euro_range_size, na_range_size, oc_range_size, africa_range_size, asia_range_size, sa_range_size)
    }
    
    result <- data.frame(
      species_name = species_name,
      range_size = as.numeric(range_size)
    )

    range_results[[species_name]] <- result

    message(paste("Range size calculated for", species_name))
  }

  range_df <- do.call(rbind, range_results)
  
  missing_species <- setdiff(rownames(data_frame), species_names)
  missing_data <- data.frame(
    species_name = missing_species,
    range_size = rep(NA, length(missing_species))
  )
  range_df <- rbind(range_df, missing_data)

  
  data_frame$species_name <- rownames(data_frame)


merged_df <- merge(data_frame, range_df, by = "species_name", all.x = TRUE)


  if (!is.null(species_names)) {
    for (species_name in species_names) {
      range_size <- range_df[range_df$species_name == species_name, "range_size"]
      cat("Range size for species", species_name, "is:", range_size, "km2\n")
  }

  return((merged_df))
}
}
