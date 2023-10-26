#' Z Transform Trait Values
#' 
#' Transform values in a dataframe into z scores to be used for rarity classification. To classify a species, measures of range size, evolutionary distinctiveness, and a chosen trait of interest must be tranformed. 
#' 
#' @param data_frame A data frame containing the columns to transform.
#' 
#' @param columns_chosen A character vector specifying the column names to transform. Default is NULL.
#' 
#' @return A data frame with Z-scores computed for the selected columns.
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' SLA_transform <- z_score_transformation(data_frame = tree_data, columns_chosen = c("range_size", "evol_dist", "SLA"))
#' 
#' transform_all <- z_score_transformation(data_frame = tree_data)
#' 
#' @export 
#' 
z_score_transformation <- function(data_frame, columns_chosen = NULL) {
  if (is.null(columns_chosen)) {
    columns_chosen <- setdiff(names(data_frame), "species_name")
  }

  z_score_df <- data_frame[, "species_name", drop = FALSE] 

  for (trait_chosen in columns_chosen) {
    
    df_filtered <- data_frame[!is.nan(data_frame[[trait_chosen]]), ]

    mean_trait <- mean(df_filtered[[trait_chosen]], na.rm = TRUE)
    sd_trait <- sd(df_filtered[[trait_chosen]], na.rm = TRUE)

    z_scores <- (df_filtered[[trait_chosen]] - mean_trait) / sd_trait

    matching_rows <- match(rownames(z_score_df), rownames(df_filtered))
    z_score_df[[trait_chosen]] <- z_scores[matching_rows]
  }

  return(z_score_df)
}