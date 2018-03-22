# converts positive and negative angles to 0-360
# asumes it is not below -360
# 390 is converted to 30, -40 to 320 etc
#' Title
#'
#' @param angle
#'
#' @return
#' @export
#'
#' @examples
#'
angle_to_360 <- function(angle){
  return((angle + 360) %% 360)
}

#' Converts angle to -180 to 180 difference
#'
#' @description
#'
#'
#' @param angle
#'
#' @return number between -180 to 180
#' @export
#'
#' @examples
angle_to_180 <- function(angle){
  angle <- ((angle + 180) %% 360) - 180
  return(angle)
}

#' Converts Pi radian angle to degrees
#'
#' @param radian
#'
#' @return
#' @export
#'
#' @examples
radian_to_angle <- function(radian){
  angle <- radian/pi * 180
  if(angle < 0) angle <- 360 + angle
  return(angle)
}

#' Converts angle to radian
#'
#' @param angle
#'
#' @return radians
#' @export
#'
#' @examples
angle_to_radian <- function(angle){
  return(angle/180 * pi)
}

#' Calculates angle from two 2d positions
#'
#' @param pos_from
#' @param pos_to
#' @param zero_vec defines which axis should correspond to 0 degrees. defaults to c(0,1) (Y axis)
#'
#' @return
#' @export
#'
#' @examples
angle_from_positions <- function(pos_from, pos_to, zero_vec = c(0,1)){
  if(length(pos_from) != 2 || length(pos_to) != 2){
    stop("ERROR:angle_from_positions, DESCRIPTION:input does not have two 2d position")
  }
  target_vector <- pos_to - pos_from
  # ATAN takes Y and X, but we want to scale it against Z axis,
  # therefore Y in carthesian, so the input is reversed
  theta <- atan2(target_vector[1], target_vector[2])
  angle <- radian_to_angle(theta)
  return(angle)
}

#' Creates x y coordinate of point position given angle X
#'
#' @param angle numeric angle in degrees (0-360)
#' @param radius vector size
#' @param center vector origin
#'
#' @return vector
#' @export
#'
#' @examples
vector_from_angle <- function(angle, radius = NULL, center = NULL){
  if(is.null(center)) center <- c(0, 0)
  if(is.null(radius)) radius <- 1
  rad <- angle_to_radian(angle)
  #TODO - the sin and cos create very small numbers that should be 1 or 0, but are like 6*10^-17
  vector <- c(center[2] + radius * sin(rad), center[1] + radius * cos(rad))
  return(vector)
}

