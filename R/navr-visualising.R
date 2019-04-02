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
#' @param obj Object
#' @param ... optional ggplot parameters
#'
#' @return
#' @export
#'
#' @examples
plot_path <- function(obj, ...){
  UseMethod('plot_path')
}

#' plots the _x and _X coordinates
#'
#' @param obj valid navr object
#' @param ... optional ggplot parameters for the path geom
#'
#' @return
#' @export
#'
#' @examples
plot_path.navr <- function(obj, ...){
  plt <- create_plot()
  #TODO - removes points that have surreal speeds
  plt <- plot_add_path(plt, obj$data$position_x, obj$data$position_y, ...)
  return(plt)
}
