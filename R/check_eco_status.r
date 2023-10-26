#' Classify Ecological Status
#' 
#' Categorize species in a data frame with z-transformed data for range size, evolutionary distinctiveness, and a selected trait into one of eight ecological statuses. Thresholds must be set in accordance with data depending on phylogenetic extent and Key = TRUE to run.
#' 
#' @param data_frame A data frame containing z-transformed values of range_size, evol_dist, and the selected trait.
#' 
#' @param species_names A character vector of specific species to classify. Default is NULL. 
#' 
#' @param trait_column The name of the z-transformed trait column to be used for classification.
#' 
#' @param trait_threshold Threshold values for the selected trait to delineate functionally unique species. Trait_threshold values should be set in accordance with phylogenetic extent. Typically trait_threshold values can be inferred using visual aids such as histograms to represent natural patterns in the data. Default is + and - 1.28.
#' 
#' @param range_size_threshold Threshold values for range size to delineate geographically large/small species. Range_size_threshold values should be set in accordance with phylogenetic extent. Typically range_size_threshold values can be inferred using visual aids such as histograms to represent natural patterns in the data. Default is -1.28.
#' 
#' @param evol_dist_threshold Threshold values for evolutionary distinctiveness to delineate evolutionarily distinct species. Evol_dist_threshold values should be set in accordance with phylogenetic extent. Typically evol_dist_threshold values can be inferred using visual aids such as histograms to represent natural patterns in the data. Default is 1.28. 
#' 
#' @param Key A logical value (TRUE/FALSE) indicating whether to classify species based on specified thresholds (TRUE) or show histograms to determine thresholds (FALSE). Default is FALSE.
#' 
#' @details The function categorizes species based on their ecological status, considering three z-transformed variables: range size, evolutionary distinctiveness, and a selected trait. The classification depends on the provided or default thresholds.
#' 
#' @return A data frame with species classifications in the "classifications" column.
#'
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' 
#' check_thresholds <- check_eco_status(data_frame = tree_data, species_names = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"))

#' maple_eco_status <- check_eco_status(data_frame = tree_data, species_names = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"), range_size_threshold = -0.6, Key= TRUE )
#' 
#' all_eco_status <- check_eco_status(data_frame = tree_data, trait_threshold = 1.1, range_size_threshold = -0.5, evol_dist_threshold = 2, Key = TRUE)
#' 
#' @export 
#' 
check_eco_status <- function(data_frame, species_names = NULL, trait_column, trait_threshold = 1.28, range_size_threshold = -1.28, evol_dist_threshold = 1.28, Key = FALSE) {
 if (!Key) {
    cat("Key must be set to TRUE to run. Please check that default thresholds (1.28) represent trends in the histograms as values will change based on phylogeny and then set Key = TRUE to proceed.\n")
    
    par(mfrow = c(1, 3))
    
    hist(data_frame$range_size, main = "Range Size Histogram", xlab = "Range Size")
    hist(data_frame$evol_dist, main = "Evol Dist Histogram", xlab = "Evol Dist")
    hist(data_frame[[trait_column]], main = "Trait Histogram", xlab = trait_column)
    
    return(NULL)
  }

  if (is.null(species_names)) {
    species_names <- unique(data_frame$species_name)
  }

  if (is.null(species_names)) {
    species_names <- unique(data_frame$species_name)
  }

  for (species_name in species_names) {
    species_data <- data_frame[data_frame$species_name == species_name, ]
    
    if (nrow(species_data) == 0) {
      cat("Species", species_name, "not found in the data frame\n")
      next  
    }
    
    if (any(is.na(species_data[[trait_column]]))) {
      cat("No", trait_column, "trait information for", species_name, "\n")
      data_frame$classifications[data_frame$species_name == species_name] <- "NA"
      next  
    }

    trait_value <- species_data[[trait_column]]
    range_size <- species_data$range_size
    evol_dist <- species_data$evol_dist


    trait_condition <- !is.na(trait_value) && (trait_value > trait_threshold || trait_value < -trait_threshold)
    range_size_condition <- !is.na(range_size) && range_size <= range_size_threshold
    evol_dist_condition <- !is.na(evol_dist) && evol_dist > evol_dist_threshold

    
    if (is.na(range_size) || is.na(evol_dist) || is.na(trait_value)) {
      classification <- "NA"
    } else {
      if (range_size_condition && evol_dist_condition && trait_condition) {
        classification <- "Classically Rare"
      } else if (range_size_condition && !evol_dist_condition && trait_condition) {
        classification <- "Endemic"
      } else if (range_size_condition && !evol_dist_condition && !trait_condition) {
        classification <- "Competitively Rare"
      } else if (range_size_condition && evol_dist_condition && !trait_condition) {
        classification <- "Relict"
      } else if (!range_size_condition && evol_dist_condition && !trait_condition) {
        classification <- "Adaptable Survivor"
      } else if (!range_size_condition && evol_dist_condition && trait_condition) {
        classification <- "Keystone"
      } else if (!range_size_condition && !evol_dist_condition && trait_condition) {
        classification <- "Potentially Invasive"
      } else {
        classification <- "Common"
      }
    }

    data_frame$classifications[data_frame$species_name == species_name] <- classification
  }
  
  return(data_frame)
}