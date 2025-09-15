#' Retrieve and Clean BIEN Trait Data for a Species
#'
#' Fetches trait data for a given plant species from the BIEN database, cleans it, and merges multiple traits into a single dataframe.
#'
#' @param species A character string specifying the species of interest.
#' @param traits A character vector specifying the trait names to retrieve from BIEN. Possible traits include:
#' - "diameter at breast height (1.3 m) (cm)"
#' - "flower color"
#' - "flower pollination syndrome"
#' - "fruit type"
#' - "inflorescence length"
#' - "leaf area (mm2)"
#' - "leaf area per leaf dry mass (m2.kg-1)"
#' - "leaf carbon content per leaf dry mass (mg.g-1)"
#' - "leaf carbon content per leaf nitrogen content"
#' - "leaf compoundness"
#' - "leaf dry mass (g)"
#' - "leaf dry mass per leaf fresh mass (mg.g-1)"
#' - "leaf fresh mass"
#' - "Leaf lamina fracture toughness"
#' - "leaf life span (months)"
#' - "leaf nitrogen content per leaf area (kg.m-2)"
#' - "leaf nitrogen content per leaf dry mass (mg.g-1)"
#' - "leaf phosphorus content per leaf area (g.m-2)"
#' - "leaf phosphorus content per leaf dry mass (mg.g-1)"
#' - "leaf photosynthetic rate per leaf area (Âµmol.m-2.s-1)"
#' - "leaf photosynthetic rate per leaf dry mass (Âµmol.g-1.s-1)"
#' - "leaf relative growth rate"
#' - "leaf stomatal conductance for H2O per leaf area (mol.H2O.m-2.s-1)"
#' - "leaf stomatal conductance per leaf area"
#' - "leaf thickness"
#' - "longest whole plant longevity (years)"
#' - "maximum fruit length"
#' - "maximum leaf length"
#' - "maximum leaf width"
#' - "maximum whole plant height"
#' - "maximum whole plant longevity"
#' - "minimum fruit length"
#' - "minimum leaf length"
#' - "minimum leaf width"
#' - "minimum whole plant height"
#' - "plant flowering begin (month)"
#' - "plant flowering duration (month)"
#' - "plant fruiting duration"
#' - "root dry mass"
#' - "seed length"
#' - "seed mass (mg)"
#' - "stem dry mass"
#' - "stem relative growth rate"
#' - "stem wood density (g.cm-3)"
#' - "vessel lumen area (mm2)"
#' - "vessel number (vessels mm-2)"
#' - "whole plant dispersal syndrome"
#' - "whole plant growth form"
#' - "whole plant growth form diversity"
#' - "whole plant height (m)"
#' - "whole plant primary juvenile period length (years)"
#' - "whole plant sexual system"
#' - "whole plant vegetative phenology"
#' - "whole plant woodiness"
#' @param remove_outliers Logical. If \code{TRUE}, removes outliers based on a specified threshold. Default is \code{FALSE}.
#' @param outlier_threshold Numeric. The number of standard deviations from the mean to classify a value as an outlier. Default is \code{3}.
#' @param author_info Logical. If \code{TRUE}, it includes authorship and contact information for data collection. Default is \code{FALSE}.
#'
#' @return A dataframe containing the cleaned trait data with columns: \code{scrubbed_species_binomial} and one column per requested trait. Optional columns include: \code{project_pi} and \code{project_pi_contacts}.
#' 
#' @details BIEN pulls from a wide variety of trait databases, making it more likely to include information on a broader range of species compared to region-specific databases. For example, the LEDA trait database (Northwestern European specific) has both SLA and seed mass data for 8 total species beloning to the Acer and Pinus genera; the BIEN trait database has SLA and seed mass data for 27 species beloning to the same genera. 
#'
#' @examples
#' \dontrun{
#' trait_df <- build_trait_data_BIEN(species = "Pinus strobus", traits = c("leaf area per leaf dry mass", "seed mass"), remove_outliers = TRUE, outlier_threshold = 2)
#'
#' print(trait_df)
#' }
#' @import BIEN
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#'  
build_trait_data_BIEN <- function(
  species,
  traits,
  remove_outliers = FALSE,
  outlier_threshold = 3,
  author_info = FALSE,
  max_retries = 3
) {
  temp_cache_dir <- file.path(tempdir(), "BIEN_cache")
  if (!dir.exists(temp_cache_dir)) dir.create(temp_cache_dir, recursive = TRUE)

  result <- tryCatch({

    merged_trait_list <- list()

    for (species_name in species) {
      for (trait in traits) {
        cache_file <- file.path(temp_cache_dir, paste0(gsub(" ", "_", species_name), "_", trait, ".rds"))
        
        if (file.exists(cache_file)) {
          message(paste("Loading cached data for", species_name, "and", trait))
          raw_data <- readRDS(cache_file)
        } else {
          message(paste("Fetching data for", species_name, "and", trait))
          
          attempt <- 1
          success <- FALSE
          while (attempt <= max_retries && !success) {
            raw_data <- tryCatch({
              BIEN_trait_traitbyspecies(species = species_name, trait = trait)
            }, error = function(e) {
              warning(paste("Error fetching data for", species_name, "and", trait, "on attempt", attempt, ":", e$message))
              return(NULL)
            })
            
            if (!is.null(raw_data)) {
              success <- TRUE
              saveRDS(raw_data, cache_file)
            } else {
              attempt <- attempt + 1
              if (attempt <= max_retries) {
                message(paste("Retrying", species_name, trait, "(", attempt, "/", max_retries, ")..."))
                Sys.sleep(2)
              } else {
                warning(paste("Failed to fetch data for", species_name, "and", trait, "after", max_retries, "attempts."))
              }
            }
          }
        }

        required_cols <- c("scrubbed_species_binomial", "trait_name", "trait_value", "unit", "method", "url_source")
        if (!is.null(raw_data) && all(required_cols %in% colnames(raw_data))) {
          merged_trait_list[[paste0(species_name, "_", trait)]] <- raw_data
        } else {
          warning(paste("Trait data for", trait, "is missing required columns or is NULL. Skipping."))
        }
      }
    }

    if (length(merged_trait_list) > 0) {
      merged_data <- bind_rows(merged_trait_list)
    } else {
      merged_data <- tibble(scrubbed_species_binomial = character())
    }

    cleaned_data <- clean_trait_data_BIEN(
      merged_data,
      remove_outliers = remove_outliers,
      outlier_threshold = outlier_threshold,
      author_info = author_info
    )

    collapsed_data <- collapse_BIEN_traits(cleaned_data)

# Clean the cache
    message("Cleaning BIEN cache directory: ", temp_cache_dir)
    unlink(temp_cache_dir, recursive = TRUE, force = TRUE)

    return(collapsed_data)

  }, error = function(e) {
    message("build_trait_data_BIEN failed: ", e$message)
    return(NULL) 
  })

  return(result)
}


#' Collapse BIEN Trait Data by Species
#'
#' Summarizes and collapses BIEN trait data by species, combining multiple trait values and author information.
#'
#' This function groups a data frame by a species identifier column, concatenates unique author information columns, 
#' and for trait columns selects the first unique non-missing value per species.
#'
#' @param df A data frame containing BIEN trait data, with one or more trait columns and optional author info columns.
#' @param species_col A string specifying the name of the species identifier column in \code{df}. Default is \code{"species_name"}.
#' @param author_cols A character vector of column names in \code{df} that contain author or project information to be concatenated. Default is \code{c("project_pi", "project_pi_contacts")}.
#'
#' @return A data frame grouped by species with author columns collapsed into semicolon-separated strings,
#'   numeric trait columns averaged per species (ignoring \code{NA}s),
#'   and non-numeric trait columns concatenated with unique values separated by semicolons.
#'
#' @importFrom dplyr group_by summarise across
#' @import tidyr
#'
#' @examples
#' \dontrun{
#'trait_data <- tibble(
#'  scrubbed_species_binomial = c("Pinus sylvestris", "Pinus sylvestris", "Pinus sylvestris",
#'                               "Quercus robur", "Quercus robur"),
#'  project_pi = c("Dr. Smith", "Dr. Smith", "Dr. Jones", "Dr. Brown", NA),
#'  project_pi_contacts = c("smith@example.com", "smith@example.com", "jones@example.com", "brown@example.com", NA),
#'  leaf_area = c(50, 50, 52, 45, 47),
#'  seed_mass = c(100, 100, NA, 120, 115),
#'  flower_color = c("yellow", "yellow", "green", "brown", "brown")
#')
#' # Use the collapse_BIEN_traits function to collapse by species
#'collapsed_df <- collapse_BIEN_traits(df = trait_data, species_col = "scrubbed_species_binomial")
#' print(collapsed_df)
#' }
#' @export
collapse_BIEN_traits <- function(df, species_col = "species_name",
                                author_cols = c("project_pi", "project_pi_contacts")) {
  
  # Only keep author columns that exist in df
  author_cols <- intersect(author_cols, names(df))
  
  # Columns that are traits (everything except species and author columns)
  trait_cols <- setdiff(names(df), c(species_col, author_cols))
  
  df_collapsed <- df %>%
    group_by(dplyr::across(all_of(species_col))) %>%
    summarise(
      # Concatenate unique non-NA authors/contact info as before
      dplyr::across(all_of(author_cols), ~ paste(unique(na.omit(.)), collapse = "; "), .names = "{.col}"),
      # For trait columns: if numeric, average ignoring NA; else concatenate unique non-NA
      dplyr::across(all_of(trait_cols), ~ {
        vals <- na.omit(.)
        if (length(vals) == 0) {
          NA
        } else if (is.numeric(vals)) {
          mean(vals)
        } else {
          paste(unique(vals), collapse = "; ")
        }
      }),
      .groups = "drop"
    )
  
  return(df_collapsed)
}
