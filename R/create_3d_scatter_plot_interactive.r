#' Create Interactive 3d Scatter Plot
#' 
#' Create an interactive scatter plot depicting the ecological status of each species in 3d space
#' 
#' @param data_frame A data frame containing species data.
#' 
#' @param trait_column The name of the trait column to use as the x-axis in the scatter plot. It is recommended to use the same trait to both classify as well as visualize.
#' 
#' @details The function checks if the specified trait column exists in the data frame and if the required columns 'range_size,' 'evol_dist,' 'classifications,' and 'species_name' are present. It then assigns colors to data points based on classifications and displays the 3D scatter plot interactively.
#' 
#' @return An interactive 3d rgl object that visualizes the three dimensions of rarity and resultant ecological status. To capture a still image of the scatter plot rgl::rgl.snapshot() is recommended.
#' 
#' @import rgl
#' 
#' @author Alivia G Nytko, \email{anytko@@vols.utk.edu}
#' 
#' @examples
#' interactive_plot <- create_3d_scatter_plot_interactive(data_frame = tree_data, trait_column = "SLA")
#' 
#' @export 
#'
create_3d_scatter_plot_interactive <- function(data_frame, trait_column) {
  if (!trait_column %in% colnames(data_frame)) {
    stop("Trait column not found in the dataframe.")
  }
  required_columns <- c("range_size", "evol_dist", "classifications", "species_name")
  if (!all(required_columns %in% colnames(data_frame))) {
    stop("Dataframe must have columns 'range_size', 'evol_dist', 'classifications', and 'species_name'.")
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
    x = data_frame[, trait_column],
    y = data_frame$range_size,
    z = data_frame$evol_dist,
    col = point_colors,  
    size = 1,  
    type = "s",  
    main = paste("3D Scatter Plot of", trait_column), 
    xlab = "Functional Uniqueness", 
    ylab = "Range Size",
    zlab = "Evolutionary Distinctiveness"  
  )

  par3d(windowRect = c(100, 100, 1100, 1100))


  legend3d("topright", legend = names(classification_colors), pch = 16, col = unlist(classification_colors), cex=1.2, inset=c(0.02), title = "Classifications")

  text3d(
    x = data_frame[, trait_column],
    y = data_frame$range_size,
    z = data_frame$evol_dist,
    text = data_frame$species_name,
    adj = c(-0.5, -0.5), 
    cex = 0.7,  
    pos = 4
  )
}
