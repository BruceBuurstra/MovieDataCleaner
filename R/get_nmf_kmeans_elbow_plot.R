#' get_nmf_kmeans_elbow_plot
#'
#' `get_nmf_kmeans_elbow_plot` returns the elbow plot of kmeans clusters for NMF
#'
#' @param nmf NMF
#' @return Returns the elbow plot of optimal kmeans clusters
#' @examples
#'
#' get_nmf_kmeans_elbow_plot(nmf) # returns elbow plot for NMF on our movie ratings data
#'
#' @export

# Finding the elbow plot for kmeans clustering
get_nmf_kmeans_elbow_plot <- function(nmf) {
  movies <- t(nmf@h)
  return(factoextra::fviz_nbclust(movies, kmeans, nstart = 100, method = "wss"))
}
