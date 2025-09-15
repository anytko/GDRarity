#' Scale by Median
#' 
#' Transform selected columns in a data frame into z-scores centered around the median using median absolute deviation (MAD).
#' This transformation is recommended when using range size and evolutionary distinctiveness for geographic and phylogenetic dimensions of rarity. 
#' 
#' @details 
#' This function calculates z-scores as:
#' \deqn{z = (x - \text{median}(x)) / \text{MAD}(x)}
#' where MAD is computed as the median of absolute deviations from the median,without applying the standard consistency constant. 
#' If `columns_chosen` is `NULL`, all columns except `"species_name"` are scaled.
#' 
#' @param data_frame A data frame containing the columns to transform. Must not contain `NA` values.
#' 
#' @param columns_chosen A character vector specifying the names of the columns to transform. If `NULL` (default), all columns except `"species_name"` are transformed.
#' 
#' @return A data frame where the selected columns have been scaled to z-scores based on the median and MAD.
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @seealso [prepare_gdrarity_axes()] where it is applied internally.
#' 
#' @examples
#' # Create dataframe of pine species with range sizes and evolutionary distinctiveness
#' pine_names <- c("Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", 
#'                 "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#' evol_dist_values <- runif(n = 9, min = -50, max = 50)
#' range_values <- runif(n = 9, min = -20000, max = 20000)
#' pine_data <- data.frame(species_name = pine_names, range_size = range_values, evol_dist = evol_dist_values)
#' 
#' # Apply median scaling to all columns except species_name
#' transform_all <- scale_by_median(data_frame = pine_data)
#' print(transform_all)
#' 
#' @export
scale_by_median <- function(data_frame, columns_chosen = NULL) {
  if (nrow(data_frame) == 0) {
    stop("Input data frame is empty.")
  }

  if (is.null(columns_chosen)) {
    columns_chosen <- setdiff(names(data_frame), "species_name")
  }
  
  if (any(is.na(data_frame))) {
    stop("Dataframe contains NA values. Please remove or impute NA values before applying the function.")
  }
  
  z_score_df <- data_frame  
  
  for (trait_chosen in columns_chosen) {
    df_filtered <- data_frame[!is.na(data_frame[[trait_chosen]]), ]
    
    median_trait <- median(df_filtered[[trait_chosen]], na.rm = TRUE)
    mad_trait <- median(abs(df_filtered[[trait_chosen]] - median_trait))
    
    if (mad_trait == 0) {
      mad_trait <- sd(df_filtered[[trait_chosen]])
      warning(sprintf("MAD is zero for trait '%s'. Falling back to standard deviation.", trait_chosen))
      
      if (mad_trait == 0) {
        mad_trait <- 1
        warning(sprintf("Standard deviation is also zero for trait '%s'. Using 1 as fallback.", trait_chosen))
      }
    }
    
    z_scores <- (df_filtered[[trait_chosen]] - median_trait) / mad_trait
    
    matching_rows <- match(rownames(z_score_df), rownames(df_filtered))
    z_score_df[[trait_chosen]] <- z_scores[matching_rows]
  }
  
  return(z_score_df)
}

