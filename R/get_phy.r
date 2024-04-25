#' Get Phylogeny
#'
#' Retreive seed plant phylogenies described in Smith & Brown 2018 from FePhyFoFum in github
#'
#' @param phy_choice Choice of seed phylogeny includes ALLMB, ALLOTB, GBMB, GBOTB
#'
#' @details The function downloads a pre-constructed phylogenetic tree from the 'big_seed_plant_trees' repository on GitHub.
#'
#' The available tree choices are as follows:
#' - 'ALLMB': GenBank and Open Tree of Life taxa with a backbone provided by Magallón et al. 2015
#' - 'ALLOTB': GenBank and Open Tree of Life taxa with a backbone provided by Open Tree of Life version 9.1
#' - 'GBMB': GenBank taxa with a backbone provided by Magallón et al. 2015
#' - 'GBOTB': GenBank taxa with a backbone provided by Open Tree of Life version 9.1
#'
#' @return A rooted phylogenetic object to be used in comparative methods
#'
#' @import ape
#'
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#'
#' @examples
#' ALLMB_phy <- get_phy(phy_choice = "ALLMB")
#'
#' ALLOTB_phy <- get_phy(phy_choice = "ALLOTB")
#'
#' GMBM_phy <- get_phy(phy_choice = "GBMB")
#'
#' GBOTB_phy <- get_phy(phy_choice = "GBOTB")
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{smithbrown2018}{EcoStatusR}
#'
#' \insertRef{kleyer2008}{EcoStatusR}
#'
#' \insertRef{Magallon2015}{EcoStatusR}
#'
#' @export
#'
get_phy <- function(phy_choice){
  if (phy_choice == "ALLMB") {
    phy <- 'https://github.com/FePhyFoFum/big_seed_plant_trees/releases/download/v0.1/v0.1.zip'
download.file(phy, file.path(tempdir(), 'v01.zip'))
unzip(file.path(tempdir(), 'v01.zip'), exdir = tempdir())
tree <- ape::read.tree(file.path(tempdir(), 'v0.1', 'ALLMB.tre'))
  } else if (phy_choice == "ALLOTB") {
    phy <- 'https://github.com/FePhyFoFum/big_seed_plant_trees/releases/download/v0.1/v0.1.zip'
download.file(phy, file.path(tempdir(), 'v01.zip'))
unzip(file.path(tempdir(), 'v01.zip'), exdir = tempdir())
tree <- ape::read.tree(file.path(tempdir(), 'v0.1', 'ALLOTB.tre'))
  } else if (phy_choice == "GBMB") {
    phy <- 'https://github.com/FePhyFoFum/big_seed_plant_trees/releases/download/v0.1/v0.1.zip'
download.file(phy, file.path(tempdir(), 'v01.zip'))
unzip(file.path(tempdir(), 'v01.zip'), exdir = tempdir())
tree <- ape::read.tree(file.path(tempdir(), 'v0.1', 'GBMB.tre'))
  } else if (phy_choice == "GBOTB") {
    phy <- 'https://github.com/FePhyFoFum/big_seed_plant_trees/releases/download/v0.1/v0.1.zip'
download.file(phy, file.path(tempdir(), 'v01.zip'))
unzip(file.path(tempdir(), 'v01.zip'), exdir = tempdir())
tree <- ape::read.tree(file.path(tempdir(), 'v0.1', 'GBOTB.tre'))
  } else {
    stop("Invalid choice. Please specify 'ALLMB', 'ALLOTB', 'GBMB', or 'GBOTB'.")
  }

  return(tree)
}

