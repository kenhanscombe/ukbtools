
#' Collects a column from a tidyverse tibble as an atomic vector.
#'
#' A function found in a
#' \href{http://stackoverflow.com/a/24730843/3819656}{Stack Overflow} discussion
#' to retrieve a  tibble column as a vector
#'
#' @param data A tidyverse tibble, tbl_df
#' @param y The column to be collected. Can be a numeric index or the column
#'   name (quoted or not)
pull <- function(data, y) {
  data[,
    if (is.name(substitute(y))) {
      deparse(substitute(y))
    } else y,
    drop = FALSE][[1]]
}
