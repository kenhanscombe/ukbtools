
#' Create a lookup table
#'
#' The lookup table is a named vector
#'
#' @param data A dataframe.
#' @param key Name of column to use as key.
#' @param value Name of column to use for value.
#'
.lookup <- function(data, key, value) {
  df <- as.data.frame(data)
  l <- df[, value]
  names(l) <- df[, key]
  return(l)
}
