#' Build Trait Data
#' 
#' Build a dataframe of trait data for various plant species of interest using LEDA trait database.
#' 
#' @param columns_to_select A character vector specifying which traits to select. Options include: SLA (specific leaf area mm^2/mg), seed_mass (mg), leaf_mass (mg), and canopy_height (m)
#' 
#' @param genera A character vector specifying genera of interest to be filtered. Default is NULL.
#' 
#' @details The function reads trait data from four different data files, including SLA (Specific Leaf Area), seed mass, leaf mass, and canopy height. It then combines these data frames into a single data frame based on the species name.
#' 
#' @return A dataframe including mean trait data for all species or species of choice in respective columns titled by trait name.
#' 
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import tibble
#' @import utils
#' @import tibble
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' 
#' Build dataframe using specific leaf area (SLA) trait data across all available species
#' all_traits <- build_trait_data(columns_to_select = "SLA")
#' 
#'Build dataframe using seed mass trait data across maple species
#' maple_traits <- build_trait_data(columns_to_select = "seed_mass", genera = "Acer_")
#' print(maple_traits)
#' 
#' Build dataframe using canopy height for major tree species
#' tree_traits <- build_trait_data(columns_to_select = "canopy_height", genera = c("Acer_", "Quercus_", "Populus_", "Ulmus_", "Pinus_", "Alnus_", "Betula_", "Salix_", "Abies_", "Fraxinus_", "Tsuga_", "Prunus_"))
#' print(tree_traits)
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{kleyer2008}{EcoStatusR}
#' 
#' @export 
#' 
build_trait_data <- function(columns_to_select, genera = NULL) {
  SLA <- read.delim("https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/SLA_und_geo_neu2.txt", skip=4, sep=';', colClasses = c(general.method="NULL", sample.size="NULL", valid="NULL", leaf.specific.method="NULL", reference="NULL", SBS.number="NULL", original.reference="NULL", country="NULL", UTM.zone="NULL", UTM.easting="NULL", UTM.northing="NULL", mean.SLA..mm.2.mg.="NULL", maximum.SLA..mm.2.mg.="NULL", minimum.SLA..mm.2.mg.="NULL", number.of.replicates="NULL", standard.deviation="NULL", standard.error="NULL", balance.error..mg.="NULL", collection.date="NULL", general.comment="NULL", plant.stage="NULL", X="NULL"), col.names = c("species", "general.method", "SLA", "sample.size", "valid", "leaf.specific.method", "reference", "SBS.number", "original.reference", "country", "UTM.zone", "UTM.easting", "UTM.northing", "mean.SLA..mm.2.mg.", "maximum.SLA..mm.2.mg.", "minimum.SLA..mm.2.mg.", "number.of.replicates", "standard.deviation", "standard.error", "balance.error..mg.", "collection.date", "general.comment", "plant.stage", "X"))

  seed_mass <- read.delim("https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/seed_mass.txt", skip=3, sep=';', colClasses = c(SBS.number="NULL", general.method="NULL", diaspore.type="NULL", sample.size="NULL", valid="NULL", median="NULL", reference="NULL", mean.SM..mg.="NULL", maximum.SM..mg.="NULL", minimum.SM..mg.="NULL", number.of.replicates="NULL", general.comment="NULL", Drying.method="NULL", original.reference="NULL", diaspore.type.code="NULL"), col.names= c("species", "SBS.number", "general.method", "diaspore.type", "seed_mass", "sample.size", "valid", "median", "reference", "mean.SM..mg.", "maximum.SM..mg.", "minimum.SM..mg.", "number.of.replicates", "general.comment", "Drying.method", "original.reference", "diaspore.type.code"))

  leaf_mass <- read.delim("https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/leaf_mass.txt", skip=3, sep=';', colClasses = c(SBS.number="NULL", general.method="NULL", sample.size="NULL", valid="NULL", reference="NULL", leaf.state="NULL", mean.LM..mg.="NULL", maximum.LM..mg.="NULL", minimum.LM..mg.="NULL", median.LM..mg.="NULL", number.of.replicates="NULL", standard.deviation="NULL", standard.error="NULL", collection.date="NULL", general.comment="NULL", original.reference="NULL"), col.names= c("species", "SBS.number", "general.method", "leaf_mass", "sample.size", "valid", "reference", "leaf.state", "mean.LM..mg.", "maximum.LM..mg.", "minimum.LM..mg.", "median.LM..mg.", "number.of.replicates", "standard.deviation", "standard.error", "collection.date", "general.comment", "original.reference"))

  canopy_height <- read.delim("https://uol.de/f/5/inst/biologie/ag/landeco/download/LEDA/Data_files/canopy_height.txt", skip = 3, sep =";", colClasses = c(SBS.number="NULL", general.method="NULL", sample.size="NULL", valid="NULL", reference="NULL", mean.CH..m.="NULL", maximum.CH..m.="NULL", minimum.CH..m.="NULL", number.of.replicates="NULL", standard.deviation="NULL", standard.error="NULL", collection.date="NULL", original.reference="NULL", country="NULL", external.support.structure="NULL"), col.names= c("species", "SBS.number", "general.method", "canopy_height", "sample.size", "valid", "reference", "mean.CH..m.", "maximum.CH..m.", "minimum.CH..m.", "number.of.replicates", "standard.deviation", "standard.error", "collection.date", "original.reference", "country", "external.support.structure"))

  trait_list <- list(SLA, seed_mass, leaf_mass, canopy_height)

  combined_data <- trait_list %>% reduce(full_join, by='species')

  full_combined_data <- combined_data %>% mutate_if(is.character, str_replace_all, ' ', '_')

  mean_data <- full_combined_data %>% group_by(species) %>% summarize(SLA = mean(SLA, na.rm = TRUE), seed_mass = mean(seed_mass, na.rm = TRUE), leaf_mass = mean(leaf_mass, na.rm = TRUE),canopy_height = mean(canopy_height, na.rm = TRUE))
  
  if (!is.null(genera)) {
    genera_species <- unique(as.character(full_combined_data$species))
    selected_species <- genera_species[grepl(paste(genera, collapse = "|"), genera_species)]
    result <- mean_data %>% filter(species %in% selected_species)
  } else {
    result <- mean_data
  }

  result <- result %>% remove_rownames %>% column_to_rownames(var = "species")

  result <- subset(result, select = columns_to_select)

  View(result)
  return(result)
}


