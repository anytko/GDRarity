#' Format Data
#' 
#' Format dataframe for compatability with other functions.
#' 
#' @param data_frame A data frame containing species data. Input data must include a "species" column and one or more trait columns.
#' 
#' @details This function replaces spaces in column names with underscores, calculates the mean values for numeric columns, and creates a new data frame with species names as row names.
#' 
#' @return A data frame with species as row names and mean values for numeric columns.
#' 
#' @import dplyr
#' @import magrittr
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' formatted_data <- format_data(flower_data)
#' 
#' @export 
#' 
format_data <- function(data_frame){

  first_data <- data_frame %>% mutate_if(is.character, str_replace_all, ' ', '_')


  numeric_columns <- sapply(first_data, is.numeric)
  
  
  mean_data <- first_data %>%
    group_by(species) %>%
    summarize(across(all_of(names(first_data)[numeric_columns]), ~ mean(., na.rm = TRUE), .names = "{.col}"))


    final_data <- mean_data %>% remove_rownames %>% column_to_rownames(var = "species")

  return(final_data)


  View(final_data)
}