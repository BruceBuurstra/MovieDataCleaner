#' optimal_knn_kmeans_clusters
#'
#' `optimal_knn_kmeans_clusters` finds the optimal number of kmeans clusters for knn
#'
#' @param data A dataset of knn imputed values to find the optimal number of kmeans clusters on
#' @return Returns the plot of optimal kmeans clusters using average silhouette width
#' @examples
#'
#' optimal_knn_kmeans_clusters(knn_imputed) # returns plot for knn on our movie ratings data
#'
#' @export

# Finding the optimal number of kmeans cluster
optimal_knn_kmeans_clusters <- function(data) {
  knn_movies <- t(data)
  return(factoextra::fviz_nbclust(knn_movies, kmeans, method = "silhouette", k.max = 20))
}
