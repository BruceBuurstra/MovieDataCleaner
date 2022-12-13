#' run_nmf_movies_clustering
#'
#' `run_nmf_movies_clustering` returns the clustered plot of kmeans clusters for NMF
#'
#' @param w number of clusters
#' @param nmf NMF
#' @return Returns the clustered plot of \code{w} kmeans clusters
#' @examples
#'
#' run_nmf_movies_clustering(2, nmf)
#' # returns kmeans 2 cluster plot for NMF on our movie ratings data
#'
#' @export

# Clustering movies using nmf and kmeans
run_nmf_movies_clustering <- function(w, nmf) {
  movies <- t(nmf@h)

  kmean <- kmeans(movies, w, nstart = 100)

  plot1 <- factoextra::fviz_cluster(kmean, data = movies,
                        geom = c("point"), ellipse.type = "euclid")

  #assign("kmean", kmean, envir = .GlobalEnv)
  return(plot1)
}
