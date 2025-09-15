#' Clean BIEN Trait Data
#'
#' Cleans and processes plant trait data from BIEN by handling missing values, removing duplicates,computing mean trait values per species, and optionally removing outliers.
#'
#' @param data A dataframe containing plant trait data. Must include the columns: \code{scrubbed_species_binomial}, \code{trait_name}, \code{trait_value}, \code{unit}, \code{method}, and \code{url_source}.
#' @param remove_outliers Logical. If \code{TRUE}, removes outliers based on a specified threshold. Default is \code{FALSE}.
#' @param outlier_threshold Numeric. The number of standard deviations from the mean to classify a value as an outlier. Default is \code{3}.
#' @param author_info Logical. If \code{TRUE}, it includes authorship and contact information for data collection. Default is \code{FALSE}.
#'
#' @return A cleaned dataframe with the columns: \code{scrubbed_species_binomial} and \code{mean_trait_value}. Optional columns include: \code{project_pi} and \code{project_pi_contacts}.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(scrubbed_species_binomial = c("Quercus robur", "Quercus robur", "Pinus sylvestris"), trait_name = c("Leaf Area", "Leaf Area", "Needle Length"), trait_value = c(20, 22, 5), unit = c("cm2", "cm2", "cm"),method = c("measurement", "measurement", "observation"), url_source = c("source1", "source2", "source3"))
#' 
#' cleaned_data <- clean_trait_data_BIEN(data, remove_outliers = FALSE)
#'
#' print(cleaned_data)
#' }
#' @import dplyr
#' @import tidyr
#' @export
#'
clean_trait_data_BIEN <- function(data, remove_outliers = FALSE, outlier_threshold = 3, author_info = FALSE) {
  
  # Ensure required columns exist
  required_cols <- c("scrubbed_species_binomial", "trait_name", "trait_value", "unit", "method", "url_source")
  if (author_info) {
    required_cols <- c(required_cols, "project_pi", "project_pi_contact")
  }
  
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("Dataframe must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Convert trait_value to numeric
  data <- data %>%
    mutate(trait_value = suppressWarnings(as.numeric(trait_value))) %>%
    drop_na(trait_value, method, url_source)  # Remove rows with NA in these columns
  
  # Remove duplicate rows based on species, trait, unit, and source
  data <- data %>%
    distinct(scrubbed_species_binomial, trait_name, unit, url_source, .keep_all = TRUE)
  
  # Clean each trait separately within species
  cleaned_data <- data %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    filter(!is.na(unit)) %>%  # Remove rows with NA in unit
    mutate(
      most_common_unit = unit[which.max(tabulate(match(unit, unique(unit))))]  # Get most common unit per trait
    ) %>%
    filter(unit == most_common_unit) %>%
    ungroup()  # Remove groupings
  
  # Remove outliers if specified
  if (remove_outliers) {
    cleaned_data <- cleaned_data %>%
      group_by(scrubbed_species_binomial, trait_name, unit) %>%  # Group by species, trait, and unit
      mutate(
        count = n(),
        species_mean = mean(trait_value, na.rm = TRUE),
        species_sd = sd(trait_value, na.rm = TRUE)
      ) %>%
      filter(
        count == 1 | abs(trait_value - species_mean) <= (outlier_threshold * species_sd)
      ) %>%
      select(-species_mean, -species_sd, -count)  # Drop intermediate columns
  }
  
  # Compute mean trait value per species per trait
  cleaned_data <- cleaned_data %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    summarize(
      mean_trait_value = mean(trait_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add author information if requested
  if (author_info) {
    author_data <- data %>%
      group_by(scrubbed_species_binomial, trait_name) %>%
      summarize(
        project_pi = paste(unique(project_pi), collapse = "; "),
        project_pi_contacts = paste(unique(project_pi_contact), collapse = "; "),
        .groups = "drop"
      )
    
    # Merge author data with cleaned trait data
    cleaned_data <- left_join(cleaned_data, author_data, by = c("scrubbed_species_binomial", "trait_name"))
  }
  
  # Reshape data to have species as rows and traits as columns
  final_data <- cleaned_data %>%
    spread(key = trait_name, value = mean_trait_value) %>%
    rename(species_name = scrubbed_species_binomial)
  
  return(final_data)
}
