#' round_to_movie_rating
#'
#' `round_to_movie_rating` gets rid of rows from a dataset that have large amounts of missing values.
#'
#' @param x A value
#' @return Returns the value of \code{x} rounded to a movie rating
#' @examples
#'
#' round_to_movie_rating(4.41) # returns 4.5
#'
#' @export

# Rounding the value to movie ratings
round_to_movie_rating <- function(x) {
  if (x < 0.25) {
    x <- 0
  } else if (x < 0.75) {
    x <- 0.5
  } else if (x < 1.25) {
    x <- 1
  } else if (x < 1.75) {
    x <- 1.5
  } else if (x < 2.25) {
    x <- 2
  } else if (x < 2.75) {
    x <- 2.5
  } else if (x < 3.25) {
    x <- 3
  } else if (x < 3.75) {
    x <- 3.5
  } else if (x < 4.25) {
    x <- 4
  } else if (x < 4.75) {
    x <- 4.5
  } else {
    x <- 5
  }
  return(x)
}
