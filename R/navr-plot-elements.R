#' Geom of plotting navr path
#'
#' @param x vector of positions
#' @param y vector of y positions
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_path <- function(x, y, ...){
  df_position <- data.frame(position_x = x, position_y = y)
  return(geom_path(data = df_position, aes(position_x, position_y), ...))
}

#' Adds image to the background of the plot
#'
#' @param plt existing plot
#' @param image_path path the png image
#' @param xlim places the image on the background
#' @param ylim
#'
#' @return
#' @export
#' @import ggplot2 png jpeg grid
#'
#' @examples
plot_add_background <- function(plt, image_path, xlim = NULL, ylim = NULL){
  #TODO - check for grid
  #Checks if png or jpg
  img <- png::readPNG(image_path)
  rast <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = T)
  if(!(is.null(xlim) | is.null(ylim))){
    #checks size of xlim and Ylim
    plt <- plt + annotation_custom(rast, xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])
  } else {
    plt <- plt + annotation_custom(rast)
  }
  return(plt)
}

#' GGplot geom to add custom PNG background to the plot
#'
#' @param image_path path the png image
#' @param xlim background poistions as vector of length 2, c(-100,400)
#' @param ylim background poistions as vector of length 2, c(-100,400)
#'
#' @return
#' @export
#'
#' @examples
geom_navr_backround <- function(image_path, xlim = NULL, ylim = NULL){
  img <- png::readPNG(image_path)
  rast <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = T)
  if(!(is.null(xlim) | is.null(ylim))){
    #checks size of xlim and Ylim
    return(annotation_custom(rast, xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]))
  } else {
    return(annotation_custom(rast))
  }
}

#' Adds shape to the background of the plot
#'
#' @param plt existing plot
#' @param x vector with pologyon X coordinates
#' @param y vector with pologyon Y coordinates
#' @param ... polygon aesthetics, such as fill, color, alpha etc.
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
plot_add_shape <- function(plt, x, y, ...){
  #VALIDATIONS
  df_poly <- data.frame(x = x, y = y)
  plt <- plt + ggplot2::geom_polygon(data = df_poly, aes(x=x, y=y), ...)
  return(plt)
}

#' Adds specified points to the given plot
#'
#' @param plt previous ggplot
#' @param ... ggplot additional params
#' @param ls list with XY vectors. eg. (list(start = c(0, 0), end = C(10, 5)))
#'
#' @import ggplot2
#' @return modified plot
#'
#' @export
plot_add_points <- function(plt, ls, ...){
  list_names <- names(ls)
  df <- data.frame(point_x = numeric(0), point_y = numeric(0), point_name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    df[i, 1] <- ls[[i]][1]
    df[i, 2] <- ls[[i]][2]
    df[i, 3] <- list_names[i]
  }
  plt <- plt + geom_point(data = df, aes(point_x, point_y), ...) +
    geom_text(data = df, aes(point_x, point_y, label = point_name))
  return(plt)
}

#' Adds arrow pointing from a point in a specified angle
#'
#' @param position data.frame. Needs to have columns x, y, angle, length, type
#' @param plt PLot to which to add the arrow
#' @return built ggplot2
#'
#' @import ggplot2
#'
#' @example
#' plt <- plot_add_direction(plt,
#'
#' @export
plot_add_direction <- function(plt, position, angle, len = 1, ...){
  ARROW_DEF <- arrow(length = unit(0.25, "cm"))
  arrow_line <- create_direction_line(position, angle, len)
  plt <- plt + geom_segment(data = arrow_line,
                            aes(x = x, y = y, xend = xend, yend = yend),
                            arrow = ARROW_DEF, ... )
  return(plt)
}

#' Checks if object has map limits variable and adds plot limits if so
#'
#' @param plt existing plot
#' @param limits list with x, y touples
#'
#' @return
#' @export
#'
#' @examples
plot_add_limits <- function(plt, limits){
  if(is.null(limits)) return(plt)
  if(!is.null(limits$x)) plt <- plt + xlim(limits$x)
  if(!is.null(limits$y)) plt <- plt + ylim(limits$y)
  return(plt)
}


#' Adds timeseries to the given plot
#'
#' @param times times on the x axis
#' @param values values on the y axis to plot
#' @param scaling type of scaling to implemnent. "none", "std", "minmax"
#' @param scale if the values should be scaled to given values
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_timeseries <- function(times, values, scaling = "none", scale = c(), ...){
  if(scaling == "std"){
    values <- scale(values)
  }
  if(scaling == "minmax"){

  }
  if(length(scale == 2)){
    #fits within a range
  }
  df <- data.frame(time = times, value=values)
  return(geom_line(data=df, aes(time, value), ...))
}


#' geom to add stat_density2d position heatmap
#'
#' @param x x positions
#' @param y y positions
#' @param bins number of bins in each direction (n parameter in statn_density)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_position_heatmap <- function(x, y, bins, ...){
  return(stat_density2d(data = df, aes(x, y, fill = stat(level)), n = bins, geom = "polygon", ...))
}
