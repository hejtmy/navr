#' Adds path to the plot data
#'
#' @param x vector of positions
#' @param y vector of y positions
#' @param plt
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
plot_add_path <- function(plt, x, y){
  df_position <- data.frame(Position.x = x, Position.y = y)
  plt <- plt + geom_path(data = df_position, aes(Position.x, Position.y))
  return(plt)
}

#' Adds image to the background of the plot
#'
#' @param plt existing plot
#' @param image_path path the png image
#' @param xlim
#' @param ylim
#'
#' @return
#' @export
#' @import ggplot2 png jpeg grid
#'
#' @examples
plot_add_image <- function(plt, image_path, xlim, ylim){
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

#' Adds shape to the background of the plot
#'
#' @param plt existing plot
#' @param x vector with pologyon X coordinates
#' @param y vector with pologyon Y coordinates
#' @param ... plygon aesthetics, such as fill, color, alpha etc.
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
#' @param size size of the dot
#' @param color dot color
#' @param ls list with XY vectors. eg. (list(start = c(0, 0), end = C(10, 5)))
#'
#' @import ggplot2
#' @return modified plot
#'
#' @export
plot_add_points <- function(plt, ls, size = 4, color = "blue"){
  list_names <- names(ls)
  df <- data.frame(point.x = numeric(0), point.y = numeric(0), point.name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    df[i, 1] <- ls[[i]][1]
    df[i, 2] <- ls[[i]][2]
    df[i, 3] <- list_names[i]
  }
  plt <- plt + geom_point(data = df, aes(point.x, point.y), size = size, color = color) +
    geom_text(data = df, aes(point.x, point.y, label = point.name))
  return(plt)
}

#' Adds arrow pointing from a point in a specified angle
#'
#' @param position_df data.frame. Needs to have columns x, y, angle, length, type
#' @param plt PLot to which to add the arrow
#' @return built ggplot2
#'
#' @import ggplot2
#'
#' @example
#' plt <- plot_add_direction(plt,
#'
#' @export
plot_add_direction <- function(plt, position, angle, len = 1, color = 'black'){
  ARROW_DEF <- arrow(length = unit(0.25, "cm"))
  arrow_line <- create_direction_line(position, angle, len)
  plt <- plt + geom_segment(data = arrow_line,
                            aes(x = x, y = y, xend = xend, yend = yend),
                            size = 1, arrow = ARROW_DEF, color = color)
  return(plt)
}
