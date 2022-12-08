#' get_knn_kmeans_elbow_plot
#'
#' `get_knn_kmeans_elbow_plot` returns the elbow plot of kmeans clusters for knn
#'
#' @param data A dataset of knn imputed values to find the optimal number of kmeans clusters on
#' @return Returns the elbow plot of optimal kmeans clusters
#' @examples
#'
#' get_knn_kmeans_elbow_plot(knn_imputed) # returns elbow plot for knn on our movie ratings data
#'
#' @export

# Finding the elbow plot for kmeans clustering
get_knn_kmeans_elbow_plot <- function(data) {
  knn_movies <- t(data)
  return(factoextra::fviz_nbclust(knn_movies, kmeans, nstart = 100, method = "wss"))
}
