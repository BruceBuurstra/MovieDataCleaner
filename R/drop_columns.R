#' drop_columns
#'
#' `drop_columns` gets rid of columns from a dataset that have large amounts of missing values.
#'
#' @param x The percentage (as a decimal) of missing values allowed in a column
#' @param data The dataset on which to remove columns from
#' @return Returns the dataset with columns having \code{x}% or more missing values removed of
#' @examples
#'
#' new_data <- drop_columns(0.9, ratings_wide) # returns our movie ratings dataset with columns having over 90% missing values removed
#'
#' @export

# Drop the columns having x or more percentage of missing values in dataset.
drop_columns <- function(x, data) {
  data <- data %>%
    purrr::discard(~sum(is.na(.x)) / length(.x) >= x)
  return(data)
}
