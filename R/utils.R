
#' Collects a column from a tidyverse tibble as an atomic vector.
#'
#' A function found in a
#' \href{http://stackoverflow.com/a/24730843/3819656}{Stack Overflow} discussion
#' to retrieve a  tibble column as a vector
#'
#' @param data A tidyverse tibble, tbl_df
#' @param y The column to be collected. Can be a numeric index or the column
#'   name (quoted or not)
#'
pull <- function(data, y) {
  data[,
       if (is.name(substitute(y))) {
         deparse(substitute(y))
       } else y,
       drop = FALSE][[1]]
}



#' Draws a grid of ggplot figures
#'
#' @param
#' \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook for R}
#'
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  n_plots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(
      seq(1, cols * ceiling(n_plots / cols)),
      ncol = cols,
      nrow = ceiling(n_plots / cols))
  }

  if (n_plots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:n_plots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(
        plots[[i]],
        vp = viewport(
          layout.pos.row = matchidx$row,
          layout.pos.col = matchidx$col
        )
      )
    }
  }
}


#' Create a lookup table
#'
#' The lookup table is a named vector
#' @param data A dataframe.
#' @param key Name of column to use as key.
#' @param value Name of column to use for value.
#'
lookup <- function(data, key, value) {
  df <- as.data.frame(data)
  l <- df[, value]
  names(l) <- df[, key]
  return(l)
}