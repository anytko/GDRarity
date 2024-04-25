#' Create Interactive 3d Scatter Plot
#' 
#' Create an interactive scatter plot depicting the ecological status of each species in 3d space
#' 
#' @param data_frame A data frame containing species data.
#' 
#' @param fun_dist A column within the data frame corresponding to the functional distinctivess of the species
#' 
#' @param evol_dist A column within the data frame corresponding to the evolutionary distinctivess of the species; should be z transformed (typically using the median and meadian absolute deviation) for ecological status classification.
#' 
#' @param range_size A column within the data frame corresponding to the range size (km^2) of the species; should be z transformed (typically using the median and meadian absolute deviation) for ecological status classification.
#' 
#' @details The function checks if the specified fun_dist, evol_dist, and range_size columns exists in the data frame and if the required columns 'classifications,' and 'species_name' are present. It then assigns colors to data points based on classifications and displays the 3D scatter plot interactively.
#' 
#' @return An interactive 3d rgl object that visualizes the three dimensions of rarity and resultant ecological status. To capture a still image of the scatter plot rgl::rgl.snapshot() is recommended.
#' 
#' @import rgl
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @seealso Please keep in mind that example classifications are randomized and therefore not representative of true ecological status.
#' 
#' @examples
#' 
#' Create dataframe including species names, range size, evolutionary distinctiveness, functional distinctiveness, and their respective eco-evolutionary classifications
#' 
#' species_names <- c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum")
#' FD_values <- runif(min = -2, max = 2, n=7)
#' range_values <- runif(min = -2, max = 2, n=7)
#' evol_dist_values <- runif(min = -2, max = 2, n=7)
#' classification <- c("Common", "Common", "Endemic", "Keystone", "Competitively Rare", "Common", "Adaptable Survivor")
#'
#' maple_data_figure <- data.frame(species_name = species_names, fun_dist = FD_values, range_size = range_values, evol_dist = evol_dist_values, classifications = classification)
#' 
#' Plot the species' classifications in 3D space
#' interactive_plot <- plot_eco_status(data_frame = maple_data_figure, fun_dist = fun_dist, range_size = range_size, evol_dist = evol_dist)
#' 
#' @export 
#'
plot_eco_status <- function(data_frame, fun_dist, evol_dist, range_size) {
  if (!fun_dist %in% colnames(data_frame)) {
    stop("Functional distinctiveness not found in the dataframe.")
  }
  if (!range_size %in% colnames(data_frame)) {
    stop("Range size not found in the dataframe.")
  }
  if (!evol_dist %in% colnames(data_frame)) {
    stop("Evolutionary distinctiveness not found in the dataframe.")
  }
  required_columns <- c("classifications", "species_name")
  if (!all(required_columns %in% colnames(data_frame))) {
    stop("Dataframe must have columns 'classifications', and 'species_name'.")
  }
  classification_colors <- c(
    "Classically Rare" = "red",
    "Endemic" = "green",
    "Competitively Rare" = "blue",
    "Relict" = "#9946cc",
    "Adaptable Survivor" = "#c17f06",
    "Potentially Invasive" = "#10cfc8",
    "Keystone" = "#a4127f",
    "Common" = "#030000"
  )
  point_colors <- classification_colors[data_frame$classifications]

  open3d(antialias = TRUE)

  plot3d(
    x = data_frame[[fun_dist]],
    y = data_frame[[range_size]],
    z = data_frame[[evol_dist]],
    col = point_colors,  
    size = 1,  
    type = "s",  
    xlab = "Functional Uniqueness", 
    ylab = "Range Size",
    zlab = "Evolutionary Distinctiveness"  
  )

  par3d(windowRect = c(100, 100, 1100, 1100))


  legend3d("topright", legend = names(classification_colors), pch = 16, col = unlist(classification_colors), cex=1.2, inset=c(0.02), title = "Classifications")

  text3d(
    x = data_frame[[fun_dist]],
    y = data_frame[[range_size]],
    z = data_frame[[evol_dist]],
    text = data_frame$species_name,
    adj = c(-0.5, -0.5), 
    cex = 0.7,  
    pos = 4
  )
}

