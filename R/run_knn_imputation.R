#' run_knn_imputation
#'
#' `run_knn_imputation` imputes the missing values into a dataset using knn
#'
#' @param n the k amount of nearest neighbors to use
#' @param data A dataset to run knn on
#' @return Returns the dataset using knn to impute missing values
#' @examples
#'
#' run_knn_imputation(5, new_data) # returns our movie ratings dataset with missing values imputed from using knn of 5
#'
#' @export

# Imputing missing values using knn
run_knn_imputation <- function(n, data) {
  data_matrix <- data
  data_matrix[is.na(data_matrix)] <- 0
  data_matrix <- data.matrix(data_matrix)
  euclidean_matrix <- matrix(nrow = ncol(data_matrix),
                             ncol = ncol(data_matrix))

  for (i in 1:ncol(data_matrix)) {
    for (j in 1:ncol(data_matrix)) {
      i1 <- which(data_matrix[, i] != 0)
      i2 <- which(data_matrix[, j] != 0)
      indx <- intersect(i1, i2)
      int1 <- data_matrix[indx, i]
      int2 <- data_matrix[indx, j]
      euc <- sqrt(sum(int1 - int2)^2)
      euclidean_matrix[i, j] <- euc
      #print(i)
      #print(j)
    }
    assign("euclidean_matrix", euclidean_matrix, envir = .GlobalEnv)
  }

  knn_imputed <- data_matrix
  for (i in 1:ncol(knn_imputed)){
    dist <- euclidean_matrix[-i,i]
    distances <- sort(dist)[1:n]
    neighbor_ind = which(dist %in% sort(dist)[1:n])
    ret <-  list(neighbor_ind, distances)
    list <- c(unlist(ret[1]))
    #print(i)
    for (j in 1:nrow(knn_imputed)) {
      if (knn_imputed[j, i] == 0) {
        knn_values <- data_matrix[j, list]
        knn_non_zero <- which(knn_values != 0)
        knn_non_zero_values <- knn_values[knn_non_zero]
        if (length(knn_non_zero_values) == 0) {
          knn_imputed[j, i] <- 0
        } else {
          avg_knn_values <- sum(knn_non_zero_values) /
            length(knn_non_zero_values)
          knn_imputed[j, i] <- avg_knn_values
        }
      } else {
        #print(j)
      }
    }
  }
  assign("knn_imputed", knn_imputed, envir = .GlobalEnv)
  return(knn_imputed)
}

#knn_round <- sapply(knn_imputed, round_to_movie_rating)
#ratings_knn_imputed <- matrix(unlist(knn_round),ncol = ncol(new_data), byrow = FALSE)
