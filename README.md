
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MovieDataCleaner

<!-- badges: start -->
<!-- badges: end -->

`MovieDataCleaner` is an R package for **cleaning missing datasets**
with **NMF imputation** and **kNN imputation**

See `pkgdown` site here:
<https://brucebuurstra.github.io/MovieDataCleaner/>

MovieDataCleaner easily:

-   drops rows and columns containg missing values
-   imputes missing values using NMF or kNN
-   clusters imputed datasets

## Installation

You can install the released version of MovieDataCleaner from
[GitHub](https://github.com) with:

``` r
devtools::install_github("BruceBuurstra/MovieDataCleaner")
```

## kNN Imputation

The `run_knn_imputation` imputes values into a dataset where there was
missing values.

``` r
library(MovieDataCleaner)
load("C:/Users/bbuur/MovieDataCleaner/data/new_data.rdata")
knn_imputed <- run_knn_imputation(5, new_data)
head(knn_imputed, c(5,5))
#>        1   2        6       10    11
#> [1,] 4.0 5.0 4.000000 5.000000 4.000
#> [2,] 4.5 4.0 3.928571 2.750000 2.000
#> [3,] 3.5 3.0 4.000000 4.166667 3.625
#> [4,] 4.0 3.0 4.000000 2.000000 3.500
#> [5,] 3.5 3.5 3.750000 5.000000 3.500
```

We can also `round_to_movie_ratings` to make sure every value is rounded
to a movie rating.

``` r
knn_round <- sapply(knn_imputed, round_to_movie_rating)
ratings_knn_imputed <- matrix(unlist(knn_round),ncol = ncol(new_data), byrow = FALSE)
head(ratings_knn_imputed, c(5,5))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  4.0  5.0    4    5  4.0
#> [2,]  4.5  4.0    4    3  2.0
#> [3,]  3.5  3.0    4    4  3.5
#> [4,]  4.0  3.0    4    2  3.5
#> [5,]  3.5  3.5    4    5  3.5
```

Below is the function `run_knn_imputation` that was written as well as
the documentation.

``` r
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
  # set missing values to 0
  data_matrix[is.na(data_matrix)] <- 0
  # convert data to data matrix
  data_matrix <- data.matrix(data_matrix)
  # create empty matrix to add euclidean distances to, square matrix with dimensions of the amount of columns from data
  euclidean_matrix <- matrix(nrow = ncol(data_matrix),
                             ncol = ncol(data_matrix))

  for (i in 1:ncol(data_matrix)) {
    for (j in 1:ncol(data_matrix)) {
      # find values from 2 different columns that are not 0
      i1 <- which(data_matrix[, i] != 0)
      i2 <- which(data_matrix[, j] != 0)
      # find where columns both have a non zero value
      indx <- intersect(i1, i2)
      # find values where columns both have a non zero value
      int1 <- data_matrix[indx, i]
      int2 <- data_matrix[indx, j]
      # find euclidean distance between columns and add to matrix
      euc <- sqrt(sum(int1 - int2)^2)
      euclidean_matrix[i, j] <- euc
      #print(i)
      #print(j)
    }
    assign("euclidean_matrix", euclidean_matrix, envir = .GlobalEnv)
  }

  knn_imputed <- data_matrix
  for (i in 1:ncol(knn_imputed)){
    # distances not including zeros on diagonal
    dist <- euclidean_matrix[-i,i]
    # sort distances
    distances <- sort(dist)[1:n]
    # find indices of where smallest distances were
    neighbor_ind = which(dist %in% sort(dist)[1:n])
    # return list of indices and distances
    ret <-  list(neighbor_ind, distances)
    # get only indices
    list <- c(unlist(ret[1]))
    #print(i)
    for (j in 1:nrow(knn_imputed)) {
      if (knn_imputed[j, i] == 0) {
        # if value is 0 find values of nearest neighbors for column
        knn_values <- data_matrix[j, list]
        # get non zero values
        knn_non_zero <- which(knn_values != 0)
        knn_non_zero_values <- knn_values[knn_non_zero]
        if (length(knn_non_zero_values) == 0) {
          # if all nearest neighbors are 0 keep missing value as 0
          knn_imputed[j, i] <- 0
        } else {
          # find average of k nearest neighbor values
          avg_knn_values <- sum(knn_non_zero_values) /
            length(knn_non_zero_values)
          # add value to knn_imputed matrix
          knn_imputed[j, i] <- avg_knn_values
        }
      } else {
        #print(j)
      }
    }
  }
  # return knn_imputed matrix
  assign("knn_imputed", knn_imputed, envir = .GlobalEnv)
  return(knn_imputed)
}
```
