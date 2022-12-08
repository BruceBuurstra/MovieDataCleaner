#' drop_rows
#'
#' `drop_rows` gets rid of rows from a dataset that have large amounts of missing values.
#'
#' @param y The percentage (as a decimal) of missing values allowed in a row
#' @param data The dataset on which to remove rows from
#' @return Returns the dataset with rows having \code{y}% or more missing values removed of
#' @examples
#'
#' new_data <- drop_rows(0.75, new_data) # returns our movie ratings dataset with rows having over 75% missing values removed
#'
#' @export

# Drop  the rows having y% or more missing values in dataset
drop_rows <- function(y, data) {
  rows <- unlist(rowSums(!is.na(data)) / ncol(data))
  bad_rows <- c()
  y <- 1 - y
  for (x in 1:length(rows)) {
    if (rows[x] < y) {
      vector <- c(x)
      bad_rows <- rbind(bad_rows, vector)
    }
  }
  if (length(bad_rows) > 0)
    data <- data[-bad_rows, ]
  return(data)
}
