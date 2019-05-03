#' Creates arrow pointing from a certain position and angle
#'
#' @param position
#' @param angle
#' @param len
#'
#' @return
#'
#' @examples
create_direction_line <- function(position, angle, len){
  vec <- c(position, position + vector_from_angle(angle) * len)
  df <- data.frame(x = vec[1], y = vec[2], xend = vec[3], yend = vec[4])
  return(df)
}


#' Creates data frame to be used to draw a circle
#'
#' @param center
#' @param radius
#' @param precision
#'
#' @return
#' @export
#'
#' @examples
make_circle <- function(center = c(0,0), radius = 1, precision = 100){
  tt <- seq(0, 2*pi, length.out = precision)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}
