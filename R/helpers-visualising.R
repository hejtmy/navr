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
