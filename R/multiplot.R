
#' Draws a grid of ggplot figures
#'
#' @param
#'  \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook for R}
#'
.multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {

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

