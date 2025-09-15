#' Rename Restriction Codes to Simplified Names
#'
#' This function takes a character vector of model codes and simplifies them by combining adjacent 'R' and 'L' suffixes for the same base letter (G, F, or P) into a single 'RL'. It also applies established names for specific restrictions.
#'
#' @param codes Character vector of model restrictions to rename.
#' 
#' @return A named character vector where the original restrictions are the names and the simplified names are the values.
#'
#' @examples
#' rename_model_code(c("GRGL", "GRFRPR", "GRGLFRFLPRPL"))
#' @export
rename_model_code <- function(codes) {
  base_letters <- c("G", "F", "P")
  
  simplify_model <- function(code) {
    parts <- unlist(strsplit(code, "(?<=.)(?=G|F|P)", perl = TRUE))  # split between codes
    new_parts <- parts
    
    for (base in base_letters) {
      r_idx <- which(new_parts == paste0(base, "R"))
      l_idx <- which(new_parts == paste0(base, "L"))
      
      if (length(r_idx) > 0 && length(l_idx) > 0) {
        # Replace the first R and L with a single RL in the position of the first of the two
        keep_idx <- min(r_idx[1], l_idx[1])
        new_parts[keep_idx] <- paste0(base, "RL")
        # Remove the other one
        remove_idx <- setdiff(c(r_idx[1], l_idx[1]), keep_idx)
        new_parts <- new_parts[-remove_idx]
      }
    }
    
    paste0(new_parts, collapse = "")
  }
  
  mapped <- setNames(vapply(codes, simplify_model, character(1)), codes)
  
  # Optional alias overrides
  alias_map <- c(
    "GRGLPR" = "EDGE",
    "GRFRPR" = "EER",
    "GRGLFRFL" = "Functional Rarity",
    "GRGLFRFLPRPL" = "GDR"
  )
  
  for (code in names(alias_map)) {
    if (code %in% codes) {
      mapped[code] <- alias_map[code]
    }
  }

  return(mapped)
}

#' Rename Columns in a Data Frame Based on Restriction Names
#'
#' This function takes a data frame and a character vector of restrictions. It renames any columns in the data frame matching the restrictions using the simplified names produced by `rename_model_code`.
#'
#' @param df A data frame containing columns to rename.
#' @param models Character vector of restrictions (model codes corresponding to column names in `df`.
#' 
#' @return The data frame with renamed columns where applicable.
#'
#' @examples
#' df <- data.frame(GRGL = 1:3, GRFRPR = 4:6)
#' rename_model_columns(df, c("GRGL", "GRFRPR"))
#' @export
rename_model_columns <- function(df, models) {
  model_cols <- models[models %in% colnames(df)]
  rename_map <- rename_model_code(model_cols)
  for (old_name in names(rename_map)) {
    new_name <- rename_map[[old_name]]
    if (!is.na(new_name)) {
      colnames(df)[colnames(df) == old_name] <- new_name
    }
  }
  return(df)
}
