#' Creates empty plot with invisible theme to clearly plot paths and points
#'
#' @return
#' @export
#'
#' @examples
create_plot <- function(){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plt <- ggplot2::ggplot()
  plt <- plt + theme_void()
  return(plt)
}

#' Title
#'
#' @param plotlist
#' @param cols
#' @param layout
#'
#' @return
#' @export
#' @import grid
#'
#' @examples
multiplot <- function(plotlist = NULL, cols = 1, layout = NULL) {
  if(!requireNamespace("grid", quietly = T)){
    stop("Cannot continue without grid")
  }
  numPlots <- length(plotlist)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plotlist[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plotlist[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
    }
  }
}
