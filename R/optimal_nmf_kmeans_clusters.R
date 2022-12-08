#' optimal_nmf_kmeans_clusters
#'
#' `optimal_nmf_kmeans_clusters` finds the optimal number of kmeans clusters for NMF
#'
#' @param nmf NMF
#' @return Returns the plot of optimal kmeans clusters using average silhouette width
#' @examples
#'
#' optimal_nmf_kmeans_clusters(nmf) # returns plot for NMF on our movie ratings data
#'
#' @export

# Finding the optimal number of kmeans cluster
optimal_nmf_kmeans_clusters <- function(nmf) {
  movies <- t(nmf@h)
  return(factoextra::fviz_nbclust(movies, kmeans, method = "silhouette", k.max = 20))
}
