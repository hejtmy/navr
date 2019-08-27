#' Creates empty plot with invisible theme to clearly plot paths and points
#'
#' @return
#'
#' @examples
create_void_plot <- function(){
  plt <- ggplot()
  plt <- plt + theme_void()
  return(plt)
}

#' Returns blank plot
#'
#' @return ggplot blank plot
#' @examples
create_minimal_plot <- function(){
  plt <- ggplot()
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
#' @param add_points if points should be explicitely noted on the path
#'
#' @return ggplot
#' @export
#'
#' @examples
plot_path.navr <- function(obj, add_points = F, ...){
  plt <- create_void_plot()
  plt$data <- obj$data[, "timestamp", drop=F]
  plt <- plt + aes(timestamp) # allows for animations later
  #TODO - removes points that have surreal speeds
  plt <- plt + geom_navr_path(obj, add_points, ...)
  plt <- plt + geom_navr_limits(obj)
  return(plt)
}


#' Plot built with plot_path function
#'
#' @param plt Needs to have timestamp x value
#' @param ...
#'
#' @return
#' @export
#' @import gganimate
#'
#' @examples
animate_path <- function(plt, ...){
  plt <- plt + transition_reveal(plt$data$timestamp)
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
  plt <- create_minimal_plot() + geom_navr_obj_timeseries(obj, "speed", scaling, scale, ...)
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
  plt <- plt + geom_navr_heatmap(obj, bins)
  return(plt)
}

