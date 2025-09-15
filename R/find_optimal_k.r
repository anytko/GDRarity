#' Find the Optimal Number of Clusters (k) Using the Elbow Method
#'
#' This function computes the optimal number of clusters (k) for a given dataset based on the within-cluster sum of squares (WCSS) values. The optimal k is determined by identifying the first k where the WCSS falls below a specified slope threshold derived from the standard deviation of the differences between consecutive WCSS values.
#'
#' @param data A data frame containing the data to be clustered.
#' @param variable A string specifying the column name within the dataframe to be clustered.
#' @param k_max An integer specifying the maximum number of clusters to consider (default is 10).
#' @param slope_factor A numeric value to scale the standard deviation of the first derivative for slope threshold determination (default is 0.5).
#' We strongly recommend using a slope >= 1 for large datasets to allow more flexibility in thresholding.
#' @param plot A logical indicating whether to generate a plot of the WCSS values against the number of clusters and the threshold value (default is FALSE). If you do not know what slope_factor to use, we recommend using plot = TRUE to visualize the data.
#'
#' @return A list containing:
#'   - `optimal_k`: An integer indicating the optimal number of clusters.
#'   - `slopes`: A data frame with the WCSS values for each k.
#'   - `slope_threshold`: The calculated slope threshold.
#'   - `plot`: (optional) A ggplot object if `plot = TRUE`.
#'
#' @examples
#' set.seed(123)
#' r_norm_data <- data.frame(normal_dist = rnorm(100, mean = 50, sd = 10)) 
#' result <- find_optimal_k(r_norm_data, variable = "normal_dist", k_max = 15, plot = TRUE)
#' print(result$optimal_k)
#' 
#' @import ggplot2
#' @export
find_optimal_k <- function(data, variable, k_max = 10, slope_factor = 0.5, plot = FALSE) {
  k_values <- 1:k_max
  wcss_values <- compute_wcss(data, variable, k_values)
  
  # Calculate first derivative (differences between consecutive WCSS values)
  first_derivative <- diff(wcss_values)
  
  # Automatically determine a slope threshold based on the standard deviation of the first derivative
  slope_threshold <- sd(first_derivative) * slope_factor  # Adjust factor as necessary
  
  # Find the optimal k: first k where the WCSS is below the slope threshold
  optimal_k_index <- which(wcss_values < slope_threshold)[1]
  
  # In case no WCSS falls below the threshold, use the last point
  if (is.na(optimal_k_index) || optimal_k_index < 2) {
    optimal_k_index <- k_max
  }

  # Prepare results
  slope_info <- data.frame(
    k = 1:k_max,  # Include all k values in slope info
    wcss_values = wcss_values
  )

  results <- list(
    optimal_k = optimal_k_index,
    slopes = slope_info,
    slope_threshold = slope_threshold
  )
  
  # Create the plot if requested
  if (plot) {
    p <- ggplot(slope_info, aes(x = k, y = wcss_values)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      geom_hline(yintercept = slope_threshold, color = "red", linetype = "dashed") +
      labs(x = "Number of Clusters (k)", 
           y = "Within-Cluster Sum of Squares (WCSS)", 
           title = "Elbow Method for Optimal K") +
      theme_minimal()
    
    results$plot <- p
  }
  
  return(results)
}
