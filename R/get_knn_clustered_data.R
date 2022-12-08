#' get_knn_clustered_data
#'
#' `get_knn_clustered_data` returns the clustered dataset of kmeans clusters for knn
#'
#' @param original an original dataset to get row ids from
#' @param data a dataset of knn imputed values
#' @return Returns the clustered dataset of kmean clusters using knn
#' @examples
#'
#' knn_clustered_data <- get_knn_clustered_data(new_data, knn_imputed) # returns kmeans 2 cluster plot for knn on our movie ratings data
#'
#' @export

# Get knn Clustered Dataset
get_knn_clustered_data <- function(original, data){
  movies <- t(original)
  knn_movies <- t(data)
  knn_kmeansClusters <- factor(knn_kmeans$cluster)

  knn_movies_final <- as.data.frame(knn_movies) %>%
    dplyr::mutate(Cluster = knn_kmeansClusters) %>%
    dplyr::mutate(movieId = row.names(movies))

  knn_movies_final2 <- merge(knn_movies_final, movie_titles, by = "movieId")
  knn_movies_final2$movieId <- as.numeric(knn_movies_final2$movieId)
  knn_clustered_data <- knn_movies_final2 %>%
    dplyr::arrange(movieId) %>%
    dplyr::select(movieId, Cluster, title, genres)
  return(knn_clustered_data)
}
