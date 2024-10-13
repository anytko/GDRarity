#' Plot a Treemap from Rarity Classification Data
#'
#' This function creates a treemap visualization from a dataframe containing a 'classifications' column. It counts the occurrences of each classification and generates a ggplot-based treemap. The user can choose to display the treemap immediately (plot = FALSE) or return it as a ggplot object for further customization (plot = TRUE).
#'
#' @param data_frame A dataframe containing a classifications' column. This column is used to categorize the data and must exist in the dataframe.
#' @param plot A logical value indicating whether to display the plot immediately (plot = FALSE). Defaults to TRUE. If TRUE the plot is stored as a ggplot object.
#'
#' @return A ggplot object representing the treemap if `plot` is TRUE or NULL if `plot` is FALSE after printing the plot.
#' 
#' @examples
#' # Example usage:
#' df <- data.frame(classifications = c("A", "B", "A", "C", "B", "A", "C"))
#' tree <- build_treemap(df, plot = TRUE)
#' tree
#'
#' @import ggplot2
#' @importFrom treemapify geom_treemap geom_treemap_text
#'
#' @export
#' 
build_treemap <- function(data_frame, plot = TRUE) {
  
  # Ensure the dataframe has the required column
  if (!"classifications" %in% names(data_frame)) {
    stop("The dataframe must contain a 'classifications' column")
  }
  
  # Count the number of occurrences for each classification
  classification_data <- as.data.frame(table(data_frame$classifications))
  names(classification_data) <- c("classifications", "Freq")
  
  # Define the color mapping for classifications
  classification_colors <- c(
    "Classically rare" = "#FFFFB3",
    "Endemic" = "#FB8072",
    "Relict" = "#FCCDE5",
    "Environmentally Rare" = "#80B1D3",
    "Indicator" = "#B3DE69",
    "High Invasive Potential" = "#FDB462",
    "Adaptable Survivor" = "#8DD3C7",
    "Common" = "#BEBADA"
  )
  
  # Build the ggplot-based treemap
  treemap_plot <- ggplot(classification_data, aes(area = Freq, fill = classifications, label = classifications)) +
    geom_treemap(color = "black", size = 0.5) + 
    geom_treemap_text(grow = TRUE, reflow = TRUE, colour = "black", 
                      place = "centre", size = 3.5) +
    scale_fill_manual(values = classification_colors) +  # Use custom colors
    theme_minimal() +                      
    theme(legend.position = "none",        
          plot.title = element_blank(),    
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          text = element_text(size = 10)) +  
    coord_fixed() + 
    annotation_custom(
      grob = grid::rectGrob(gp = grid::gpar(col = "black", fill = NA, lwd = 2)), 
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

  # If plot = TRUE, return the ggplot object for further use
  if (plot) {
    return(treemap_plot)
  }
  
  # If plot = FALSE, print the plot to the plotting window
  print(treemap_plot)
  
  return(NULL)
}
