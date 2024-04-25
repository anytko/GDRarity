#' Scale by Median
#' 
#' Transform values in a dataframe into z scores around the median. To classify species status, measures of range size and evolutionary distinctiveness must be tranformed. 
#' 
#' @details Chosen columns are scaled using the median and median absolute deviation. This transformation is recommend for range size and evolultionary distinctiveness when classifying the ecological status of species using EcoStatusR::check_eco_status().
#' 
#' @param data_frame A data frame containing the columns to transform.
#' 
#' @param columns_chosen A character vector specifying the column names to transform. Default is NULL.
#' 
#' @return A scaled data frame with Z-scores computed using the median and medain absolute deviation (MAD) for the selected columns.
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @seealso Please keep in mind that these values in the examples are randomized and therefore not representative of true ecological status.
#' 
#' @examples
#' 
#' Create dataframe of pine species with range sizes and evolutionary distinctiveness
#' pine_names <- c("Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#' evol_dist_values <- runif(min = -50, max = 50, n=9)
#' range_values <- runif(min = -20000, max = 20000, n=9)
#'
#' pine_data <- data.frame(species_name = pine_names, range_size = range_values, evol_dist = evol_dist_values)
#'
#' transform_all <- scale_by_median(data_frame = pine_data)
#' print(transform_all)
#' 
#' @export 
#' 
scale_by_median <- function(data_frame, columns_chosen = NULL) {
  if (is.null(columns_chosen)) {
    columns_chosen <- setdiff(names(data_frame), "species_name")
  }

  z_score_df <- data_frame  

  for (trait_chosen in columns_chosen) {
    df_filtered <- data_frame[!is.na(data_frame[[trait_chosen]]), ]

    median_trait <- median(df_filtered[[trait_chosen]], na.rm = TRUE)
    mad_trait <- median(abs(df_filtered[[trait_chosen]] - median_trait))

    z_scores <- (df_filtered[[trait_chosen]] - median_trait) / mad_trait

    matching_rows <- match(rownames(z_score_df), rownames(df_filtered))
    z_score_df[[trait_chosen]] <- z_scores[matching_rows]
  }

  return(z_score_df)
}
