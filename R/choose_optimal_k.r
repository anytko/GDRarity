#' Choose Optimal Number of Clusters (k)
#' 
#' Prompt the user to input the optimal number of clusters (k) for a given variable.
#' 
#' @param variable The name of the variable (range size, evolutionary distinctiveness, or functional distinctiveness) for which the optimal number of clusters is being chosen.
#' 
#' @return An integer representing the optimal number of clusters (k).
#' 
#' @details This function prompts the user to input the optimal number of clusters (k) for a given variable. The user input should be a numeric value representing the desired number of clusters. The function then saves and returns this value.
#' 
#' @examples
#' 
#' Enter the chosen k value for stem length (using a method such as plotting the elbow curve)
# optimal_k <- choose_optimal_k("Stem Length")
#' 
#' @export
#' 
choose_optimal_k <- function(variable) {
  k <- as.numeric(readline(prompt = paste("Enter the optimal number of clusters (k) for", variable, ": ")))
  return(k)
}

