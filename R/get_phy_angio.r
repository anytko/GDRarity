#' Get Phylogeny of Angiosperms
#'
#' Retreive seed plant phylogenies described in Smith & Brown 2018 from FePhyFoFum in github
#'
#' @param phy_choice Character string specifying which seed plant phylogeny to download. 
#'   Must be one of:
#'   \itemize{
#'     \item `"ALLMB"`: GenBank and Open Tree of Life taxa with a backbone from Magallón et al. (2015)
#'     \item `"ALLOTB"`: GenBank and Open Tree of Life taxa with a backbone from Open Tree of Life version 9.1
#'     \item `"GBMB"`: GenBank taxa with a backbone from Magallón et al. (2015)
#'     \item `"GBOTB"`: GenBank taxa with a backbone from Open Tree of Life version 9.1
#'   }
#'
#' @details 
#' The function downloads a pre-constructed phylogenetic tree from the 
#' \href{https://github.com/FePhyFoFum/big_seed_plant_trees}{big_seed_plant_trees} 
#' GitHub repository, unzips it to a temporary directory, and reads the `.tre` file 
#' corresponding to the selected `phy_choice`. 
#'
#' @return A rooted phylogenetic tree as a `phylo` object to be used in comparative methods
#'
#' @import ape
#' @importFrom utils download.file unzip
#' @importFrom Rdpack reprompt
#'
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#'
#' @examples
#' \dontrun{
#' ALLMB_phy <- get_phy_angio(phy_choice = "ALLMB")
#'
#' ALLOTB_phy <- get_phy_angio(phy_choice = "ALLOTB")
#'
#' GMBM_phy <- get_phy_angio(phy_choice = "GBMB")
#'
#' GBOTB_phy <- get_phy_angio(phy_choice = "GBOTB")
#' }
#'
#' @references
#' \insertRef{smithbrown2018}{GDRarity}
#' \insertRef{kleyer2008}{GDRarity}
#' \insertRef{Magallon2015}{GDRarity}
#'
#' @export
#'
get_phy_angio <- function(phy_choice){
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

