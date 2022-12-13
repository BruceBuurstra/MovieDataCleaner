#' run_knn_movies_clustering
#'
#' `run_knn_movies_clustering` returns the clustered plot of kmeans clusters for knn
#'
#' @param v number of clusters
#' @param data A dataset of knn imputed values
#' @return Returns the clustered plot of \code{v} kmeans clusters
#' @examples
#'
#' run_knn_movies_clustering(2, knn_imputed)
#' # returns kmeans 2 cluster plot for knn on our movie ratings data
#'
#' @export

# Clustering movies using knn and kmeans
run_knn_movies_clustering <- function(v, data) {
  knn_movies <- t(data)

  knn_kmeans <- kmeans(knn_movies, v, nstart = 100)

  plot1 <- factoextra::fviz_cluster(knn_kmeans, data = knn_movies,
                        geom = c("point"), ellipse.type = "euclid")

  #assign("knn_kmeans", knn_kmeans, envir = .GlobalEnv)
  return(plot1)
}
