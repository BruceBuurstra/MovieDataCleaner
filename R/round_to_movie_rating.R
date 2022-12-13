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
  ifelse(x > 5, 5, round(x))
}
