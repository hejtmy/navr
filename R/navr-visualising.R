#' Creates empty plot with invisible theme to clearly plot paths and points
#'
#' @return
#'
#' @examples
create_void_plot <- function(){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plt <- ggplot2::ggplot()
  plt <- plt + theme_void()
  return(plt)
}

#' Returns blank plot
#'
#' @return ggplot blank plot
#' @examples
create_minimal_plot <- function(){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plt <- ggplot2::ggplot()
  plt <- plt + theme_minimal()
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
#' @return ggplot
#' @export
#'
#' @examples
plot_path.navr <- function(obj, ...){
  plt <- create_void_plot()
  #TODO - removes points that have surreal speeds
  plt <- plt + geom_navr_path(obj$data$position_x, obj$data$position_y, ...)
  return(plt)
}

#' Adds path to the plot data
#'
#' @param obj navr object to which
#' @param plt
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
plot_add_path <- function(plt, obj, ...){
  .Deprecated("geom_navr_path")
  plt <- plt + geom_navr_path(obj$data$position_x, obj$data$position_y, ...)
  return(plt)
}


#' PLots speed values in time
#'
#' @param obj
#' @param scaling type of scaling to implement, possibilities are "std", "minmax"
#' @param scale if the values should be scaled to certain values - needs vector of length 2 c(0,1)
#' @param ...
#' @return ggplot
#'
#' @return
#' @export
#'
#' @examples
plot_speed <- function(obj, scaling = "none", scale = c(), ...){
  UseMethod("plot_speed")
}

#' PLots speed values in time
#' @export
plot_speed.navr <- function(obj, scaling = "none", scale = c(), ...){
  #validates
  if(!has_column(obj$data, "speed")){
    stop("Cannot plot speeds. No speed column present. Have you run add_speeds function on your object?")
  }
  #plot value
  plt <- create_minimal_plot() + geom_navr_timeseries(obj$data$time_since_start, obj$data$speed, scaling, scale, ...)
  return(plt)
}

#' Plots position heatmap with given granularity
#'
#' @param obj object
#' @param ... optional ggplot parameters
#' @param bins number of bins to segment the area
#'
#' @return
#' @export
#'
#' @examples
plot_position_heatmap <- function(obj, bins, ...){
  UseMethod('plot_position_heatmap')
}

#' Plots position heatmap with given granularity
#'
#' @param ... optional ggplot parameters
#' @param obj object
#' @param bins number of bins to segment the area
#'
#' @return ggplot
#' @export
#'
#' @examples
plot_position_heatmap.navr <- function(obj, bins = 25, ...){
  plt <- create_void_plot()
  plt <- plot_add_position_heatmap(plt, obj, bins, ...)
  return(plt)
}

# https://stackoverflow.com/questions/48282989/show-only-high-density-areas-with-ggplot2s-stat-density-2d
#' PLots heatmap using stat_density2d
#'
#' @param plt plot to which to add
#' @param bins number of bins in each direction (n parameter in statn_density)
#' @param ...
#' @param obj navr object which to plot
#'
#' @return
#' @export
#'
#' @examples
plot_add_position_heatmap <- function(plt, obj, bins, ...){
  plt <- plt + geom_position_heatmap(obj$data$position_x, obj$data$position_y, bins)
  return(plt)
}
