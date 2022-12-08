#' run_nmf_imputation
#'
#' `run_nmf_imputation` reduces the dimension of dataset using nmf
#'
#' @param z The rank of the NMF
#' @param data A dataset to run NMF on
#' @return Returns the dataset using NMF of rank \code{z} to impute missing values
#' @examples
#'
#'  # returns our movie ratings dataset with missing values imputed from an NMF of rank 3
#'
#' @export

# Reducing the dimension of dataset using nmf
run_nmf_imputation <- function(z, data) {
  data_matrix <- data
  data_matrix[is.na(data_matrix)] <- 0
  data_matrix <- data.matrix(data_matrix)

  nmf <- RcppML::nmf(data_matrix, z, mask = "zeros")

  data_nmf_imputed <- as.matrix(nmf@w %*% Matrix::Diagonal(x = nmf@d) %*% nmf@h)
  assign("nmf", nmf, envir = .GlobalEnv)
  return(data_nmf_imputed)
}

#run_nmf_imputation(3, new_data)
