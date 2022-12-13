#' get_nmf_clustered_data
#'
#' `get_nmf_clustered_data` returns the clustered dataset of kmeans clusters for NMF
#'
#' @param nmf NMF
#' @param kmean kmean
#' @return Returns the clustered dataset of kmean clusters using NMF
#' @examples
#'
#' nmf_clustered_data <- get_nmf_clustered_data(nmf, kmean)
#' # returns kmeans clustered dataset of movies using NMF on our movie ratings data
#'
#' @export

# Get NMF Clustered Dataset
get_nmf_clustered_data <- function(nmf, kmean) {
  movies <- t(nmf@h)
  kmeansClusters <- factor(kmean$cluster)

  movies_final <- as.data.frame(movies) %>%
    dplyr::mutate(Cluster = kmeansClusters) %>%
    dplyr::mutate(movieId = row.names(movies))

  movies_final2 <- merge(movies_final, movie_titles, by = "movieId")
  movies_final2$movieId <- as.numeric(movies_final2$movieId)
  nmf_clustered_data <- movies_final2 %>%
    dplyr::arrange(movieId) %>%
    dplyr::select(movieId, Cluster, title, genres)
  return(nmf_clustered_data)
}
