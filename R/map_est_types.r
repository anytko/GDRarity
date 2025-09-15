#' Map Rarity Types to Established Restrictions.
#'
#' This function maps restriction-specific rarity types to restrictions "EER" and "Functional Rarity". It uses standardized restriction names created via `rename_model_code()`.
#'
#' @param model A character string representing the restriction (e.g., "GRFRPR", "GRGLFRFL").
#' @param label A character string representing the presence or absence of rarity for each dimension. 
#'
#' @return A character string with the rarity type if a match exists to a established restriction.
#'
#' @seealso [rename_model_code()] for how model codes are standardized.
#'
#' @examples
#'map_est_types("GRFRPR", "GR+FR-PR+")
#'map_est_types("GRGLFRFL", "GR-GL-FR-FL-")
#'
#' @export
map_est_types <- function(model, label) {
  # Normalize model name
  model_renamed <- rename_model_code(model)

  # Mapping for EER
  eer_map <- c(
    "GR-FR+PR+" = "Indicator",
    "GR-FR-PR+" = "Adaptable survivor",
    "GR+FR+PR+" = "Classically Rare",
    "GR+FR-PR+" = "Relict",
    "GR-FR+PR-" = "High Invasive Potential",
    "GR-FR-PR-" = "Common",
    "GR+FR+PR-" = "Endemic",
    "GR+FR-PR-" = "Environmentally Rare"
  )

  # Mapping for Functional Rarity
  fr_map <- c(
    "GR+GL+FR+FL+" = "A",  "GR+GL-FR+FL+" = "B",
    "GR-GL+FR+FL+" = "C",  "GR-GL-FR+FL+" = "D",
    "GR+GL+FR+FL-" = "Impossible",  "GR+GL-FR+FL-" = "Impossible",
    "GR-GL+FR+FL-" = "Impossible",  "GR-GL-FR+FL-" = "Impossible",
    "GR+GL+FR-FL+" = "E",  "GR+GL-FR-FL+" = "F",
    "GR-GL+FR-FL+" = "G",  "GR-GL-FR-FL+" = "H",
    "GR+GL+FR-FL-" = "I",  "GR+GL-FR-FL-" = "J",
    "GR-GL+FR-FL-" = "K",  "GR-GL-FR-FL-" = "L"
  )

  # Return the mapped label if it's a special model
  if (model_renamed == "EER") {
    return(ifelse(!is.na(eer_map[label]), eer_map[label], label))
  } else if (model_renamed == "Functional Rarity") {
    return(ifelse(!is.na(fr_map[label]), fr_map[label], label))
  } else {
    return(label)  # Leave unchanged for other models
  }
}
