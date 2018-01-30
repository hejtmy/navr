#' Title
#'
#' @param plt
#' @param df_pos data.frame with Position.X, Position.Y columns
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
plot_add_path <- function(plt, df_position){
  plt <- plt + geom_path(data = df_position, aes(Position.X, Position.Y))
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
