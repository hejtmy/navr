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


#' Completely empty theme for clear plotting
#'
#' @param base_size
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
theme_invisible <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_void,
    axis.text.x =       theme_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = NA, size = 0.2),
    axis.title.x =      theme_text(colour = NA,size = base_size, vjust = 1),
    axis.title.y =      theme_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour=NA),
    legend.key =        theme_rect(colour = NA),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(colour = NA,size = base_size * 0.8),
    legend.title =      theme_text(colour = NA,size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_rect(fill = NA, colour = NA),
    panel.border =      theme_rect(fill = NA, colour=NA),
    panel.grid.major =  theme_line(colour = NA, size = 0.2),
    panel.grid.minor =  theme_line(colour = NA, size = 0.5),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = NA, colour = NA),
    strip.text.x =      theme_text(colour = NA,size = base_size * 0.8),
    strip.text.y =      theme_text(colour = NA,size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(colour = NA,size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

