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

#' Plots path of dataset
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
plot_path <- function(obj){
  UseMethod('plot_path')
}

#' plots the _x and _X coordinates
#'
#' @param obj valid navr object
#'
#' @return
#' @export
#'
#' @examples
plot_path.navr <- function(obj){
  plt <- create_plot()
  #removes
  plt <- plot_add_path(plt, obj$data$position_x, obj$data$position_y)
  return(plt)
}
