#' Check Eco-Evolutionary Rarity (EER) Status with K Parameter
#'
#' This function categorizes species into eco-evolutionary status based on the provided trait data and custom thresholds based on specified optimal K clusters.
#'
#' @param data_frame A data frame containing scaled (typically using the median and median absolute deviation) values of range size, evolutionary distinctiveness, and functional distinctiveness.
#' 
#' @param range_size_col A column within the data frame corresponding to the range size (km^2) of the species; should be z transformed (typically using the median and meadian absolute deviation) for ecological status classification. Use the name of the column in "".
#' 
#' @param mean_evol_dist_col A column within the data frame corresponding to the evolutionary distinctivess of the species; should be z transformed (typically using the median and meadian absolute deviation) for ecological status classification. Use the name of the column in "".
#' 
#' @param fun_dist_col A column within the data frame corresponding to the functional distinctivess of the respective species. Use the name of the column in "".
#' 
#' @param range_size_K The optimal number of clusters (K) for range size.
#' 
#' @param mean_evol_dist_K The optimal number of clusters (K) for mean evolutionary disctinctiveness.
#' 
#' @param fun_dist_K The optimal number of clusters (K) for functional disctinctiveness.
#' @return A data frame with an additional column "classifications" indicating the ecological status of each species.
#' @examples
#' 
#' Create a dataframe of maple, ash, and pine species with range size, evolutionary distinctiveness, and functional distinctiveness
#' 
#' species_names <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#' 
#' FD_values <- runif(min = -2, max = 2, n=23)
#' 
#' range_values <- runif(min = -2, max = 2, n=23)
#' 
#' evol_dist_values <- runif(min = -2, max = 2, n=23)
#' 
#' forest_data <- data.frame(species_name = species_names, fun_dist = FD_values, range_size = range_values, mean_evol_dist = evol_dist_values)
#' 
#' Find the "elbows" in the k means plots below
#' elbow_plot(forest_data, "range_size")
#' elbow_plot(forest_data, "mean_evol_dist")
#' elbow_plot(forest_data, "fun_dist")
#'
#' eco_stat <- check_EER_status_k(data_frame = forest_data, fun_dist_col = "fun_dist", range_size_col = "range_size", mean_evol_dist_col = "mean_evol_dist", range_size_k = 3, mean_evol_dist_k = 3, fun_dist_k = 3)
#' 
#' print(eco_stat)
#' @export

check_EER_status_k <- function(data_frame, range_size_col, mean_evol_dist_col, fun_dist_col, range_size_k, mean_evol_dist_k, fun_dist_k ) {
  
  # Initialize the classification column with NA
  data_frame$classifications <- NA

  # Identify species with any NA values and assign "NA" classification
  species_with_na <- unique(data_frame$species_name[ 
    is.na(data_frame[[range_size_col]]) |
    is.na(data_frame[[mean_evol_dist_col]]) |
    is.na(data_frame[[fun_dist_col]])
  ])
  
  if (length(species_with_na) > 0) {
    data_frame$classifications[data_frame$species_name %in% species_with_na] <- "NA"
  }

  # Check if all species are classified as "NA"
if (all(is.na(data_frame[[range_size_col]])) | 
    all(is.na(data_frame[[mean_evol_dist_col]])) | 
    all(is.na(data_frame[[fun_dist_col]]))) {
  stop("All values in one or more columns are NA. Cannot perform classification.")
}

  # Filter out species with NA classifications for k-clustering
  data_to_cluster <- data_frame[is.na(data_frame$classifications), ]
  
  # Choose optimal K for range size
  k_range_size <- range_size_k
  range_size_ranges <- define_custom_k_ranges(data.frame(range_size = data_to_cluster[[range_size_col]]), range_size_col, k_range_size)$custom_ranges
  range_size_threshold <- range_size_ranges[[1]][2]

  # Choose optimal K for mean evolutionary distance
  k_mean_evol_dist <- mean_evol_dist_k
  mean_evol_dist_ranges <- define_custom_k_ranges(data.frame(mean_evol_dist = data_to_cluster[[mean_evol_dist_col]]), mean_evol_dist_col, k_mean_evol_dist)$custom_ranges
  evol_dist_threshold <- mean_evol_dist_ranges[[length(mean_evol_dist_ranges)]][1]

  # Choose optimal K for functional distance
  k_fun_dist <- fun_dist_k
  fun_dist_ranges <- define_custom_k_ranges(data.frame(fun_dist = data_to_cluster[[fun_dist_col]]), fun_dist_col, k_fun_dist)$custom_ranges
  fun_dist_threshold <- fun_dist_ranges[[length(fun_dist_ranges)]][1]

  # Categorize remaining species into statuses based on the adjusted thresholds and custom ranges
  for (species_name in unique(data_to_cluster$species_name)) {
    species_data <- data_to_cluster[data_to_cluster$species_name == species_name, ]
    
    if (nrow(species_data) == 0) {
      cat("Species", species_name, "not found in the data frame\n")
      next
    }
    
      trait_value <- species_data[[fun_dist_col]]
    range_size_value <- species_data[[range_size_col]]
    evol_dist_value <- species_data[[mean_evol_dist_col]]

    trait_condition <- !is.na(trait_value) && (trait_value > fun_dist_threshold || trait_value < -fun_dist_threshold)
    range_size_condition <- !is.na(range_size_value) && range_size_value <= range_size_threshold
    evol_dist_condition <- !is.na(evol_dist_value) && evol_dist_value > evol_dist_threshold

    if (is.na(range_size_value) || is.na(evol_dist_value) || is.na(trait_value)) {
      classification <- "NA"
    } else {
      if (range_size_condition && evol_dist_condition && trait_condition) {
        classification <- "Classically Rare"
      } else if (range_size_condition && !evol_dist_condition && trait_condition) {
        classification <- "Endemic"
      } else if (range_size_condition && !evol_dist_condition && !trait_condition) {
        classification <- "Environmentally Rare"
      } else if (range_size_condition && evol_dist_condition && !trait_condition) {
        classification <- "Relict"
      } else if (!range_size_condition && evol_dist_condition && !trait_condition) {
        classification <- "Adaptable Survivor"
      } else if (!range_size_condition && evol_dist_condition && trait_condition) {
        classification <- "Indicator"
      } else if (!range_size_condition && !evol_dist_condition && trait_condition) {
        classification <- "High Invasive Potential"
      } else {
        classification <- "Common"
      }
    }

    data_frame$classifications[data_frame$species_name == species_name] <- classification
  }

  return(data_frame)
}


