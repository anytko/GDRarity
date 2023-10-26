#' Calculate Evolutionary Distinctiveness
#' 
#' Calculate the evolutionary distinctiveness using picante::evol.distinct() of all species across a chosen phylogenetic extent. 
#' 
#' @param phylogeny An ape phylogenetic tree object.
#' 
#' @param data_frame A data frame containing species data with a 'species_name' column.
#' 
#' @details The function takes a phylogenetic tree (phylogeny) and a data frame (data_frame) as inputs. It prunes the phylogenetic tree to retain only the species listed in the data frame and then calculates the evolutionary distinctiveness (evol_dist) using the fair proportion (FP) index for each species. The results are merged with the original data frame, creating a new data frame with evolutionary distinctiveness values added as a new column. 
#' 
#' @return A data frame containing species data with an additional 'evol_dist' column.
#' 
#' @import ape
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' GBMB_ED <- calculate_evol_dist(phylogeny = GBMB_phy, data_frame = tree_data)
#' 
#' ALLOTB_ED <- get_phy(phylogeny = ALLOTB_phy, data_frame = flower_data)
#' @export 
#' 
calculate_evol_dist <- function(phylogeny, data_frame) {
 
new_df <- data_frame %>% remove_rownames %>% column_to_rownames(var="species_name")

rownames(new_df) <- gsub("_+", "_", gsub(" ", "_", rownames(new_df)))

data_species_names <- unique(rownames(new_df))
  
  
  pruned_phylogeny <- drop.tip(phy, setdiff(phy$tip.label, data_species_names))
  
  phylogeny_evol_dist <- picante::evol.distinct(pruned_phylogeny, type = "fair.proportion")

  colnames(phylogeny_evol_dist) <- c("species_name", "evol_dist")

  data_frame$species_name <- gsub(" ", "_", data_frame$species_name)

merged_data <- merge(data_frame, phylogeny_evol_dist, by.x = "species_name", all.x = TRUE)


   return(merged_data)

}