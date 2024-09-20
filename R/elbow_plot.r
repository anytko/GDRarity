#' Display Elbow Plot
#' 
#' Plot the Within-Cluster Sum of Squares (WCSS) against the number of clusters (k) to determine the optimal value of k using the Elbow Method.
#' 
#' @param data A data frame containing the data for which the elbow plot is to be displayed.
#' 
#' @param variable The variable in the data frame for which the elbow plot is to be generated. Use the name of the column in "".
#' 
#' @param k_max The maximum number of clusters (k) to consider. Default is 10. 
#' 
#' @param ggplot Logical indicator. Default is FALSE. If FALSE the function returns a R base plot. If TRUE the function returns a ggplot. 
#' 
#' @details The Elbow Method helps in finding the optimal number of clusters (k) by plotting the Within-Cluster Sum of Squares (WCSS) against different values of k. The "elbow" point in the plot, where the rate of decrease in WCSS slows down, indicates the optimal value of k. This is often visually determined by finding the "elbow" in the plot. 
#' 
#' @seealso Use this function to visualize the optimal k value before computing eco-evolutionary statusing using GeoFunPhy::check_EER_status_k().
#' 
#' @import ggplot2
#' 
#' @examples
#' 
#' Create dataframe of maple, ash, and pine species with range size, evolutionary distinctiveness, and functional distinctiveness values 
#' species_names <- c("Abies_alba", "Abies_grandis", "Abies_nordmanniana", "Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum", "Fraxinus_angustifolia", "Fraxinus_excelsior", "Fraxinus_ornus", "Fraxinus_pennsylvanica", "Pinus_banksiana", "Pinus_cembra", "Pinus_nigra", "Pinus_pinaster", "Pinus_pinea", "Pinus_ponderosa", "Pinus_strobus", "Pinus_sylvestris", "Pinus_uncinata")
#' 
#' FD_values <- runif(min = -2, max = 2, n=23)
#' 
#' range_values <- runif(min = -2, max = 2, n=23)
#' 
#' evol_dist_values <- runif(min = -2, max = 2, n=23)
#' 
#' forest_data <- data.frame(species_name = species_names, fun_dist = FD_values, range_size = range_values, mean_evol_dist = evol_dist_values)
#' 
#' Plot the elbow plot for range size
#' elbow_plot(forest_data, "range_size")
#' 
#' Plot the elbow plot for average evolutionary distinctivess
#' elbow_plot(forest_data, "mean_evol_dist", 8)
#' 
#' Plot the elbow plot for functional distinctiveness
#' fun_dist_elbow <- elbow_plot(forest_data, "fun_dist", 10, ggplot = TRUE)
#' 
#' @export
elbow_plot <- function(data, variable, k_max = 10, ggplot = FALSE) {
  k_values <- 1:k_max
  wcss_values <- compute_wcss(data, variable, k_values)
  
  if (ggplot) {
    # Create ggplot object
    p <- ggplot(data.frame(k = k_values, WCSS = wcss_values), aes(x = k, y = WCSS)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = paste("Elbow Method for", variable),
           x = "Number of Clusters (k)",
           y = "Within-Cluster Sum of Squares (WCSS)") +
      theme_minimal()
    
    return(p)
  } else {
    # Base R plot
    plot(k_values, wcss_values, type = "b", pch = 19, frame = FALSE, col = "blue", 
         main = paste("Elbow Method for", variable), 
         xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)")
  }
}
