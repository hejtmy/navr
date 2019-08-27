#' Creates arrow pointing from a certain position and angle
#'
#' @param position
#' @param angle
#' @param len direction line length
#'
#' @return
#' @noRd
create_direction_line <- function(position, angle, len){
  vec <- c(position, position + vector_from_angle(angle) * len)
  df <- data.frame(x = vec[1], y = vec[2], xend = vec[3], yend = vec[4])
  return(df)
}

#' Creates data frame to be used to draw a circle
#'
#' @param center center of the circle as a numeric(2)
#' @param radius radius of a circle
#' @param precision how many points should hte circle be made of
#'
#' @return data.frame with circle coordinates
#' @noRd
make_circle <- function(center = c(0,0), radius = 1, precision = 100){
  tt <- seq(0, 2*pi, length.out = precision)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}
