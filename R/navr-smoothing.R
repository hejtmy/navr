#' Smooths positions of a given boject
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
smooth_positions <- function(obj, ...){
  UseMethod("smooth_positions")
}

#' Smooth positions in a navr object
#'
#' @param obj navr object
#'
#' @param type string: median, approx, spline
#' @param ....
#'
#' @return
#' @export
#'
#' @examples
smooth_positions.navr <- function(obj, type, ....){
  if(is.null(ls)) return(NULL)
  obj$data$position_x <- smooth_vector(obj$data$position_x, type, ...)
  obj$data$position_y <- smooth_vector(obj$data$position_y, type, ...)
  return(obj)
}

#' Smooths speed
#'
#' @param obj object to perfrom speed smoothing on
#' @param type what type of smoothing to perform
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
smooth_speed <- function(obj, type, ...){
  UseMethod("smooth_speed")
}

#' Smooths speed vector in the navr object
#'
#' @param obj navr object
#' @param type type of smoothing
#' @param ... optional parameters for given smoothing, see `smooth_vector` function
#'
#' @return modified navr object
#' @export
#'
#' @examples
smooth_speed.navr <- function(obj, type, ...){
  obj$data$speed <- smooth_vector(obj$data$speed, type, ...)
  return(obj)
}


#' Smooths positions of given vector
#'
#' @param df Data frame to modify
#' @param x vector to smooth
#' @param type median, approx, spline
#' @param ... optional parameters for the smoothing function (see below)
#'
#' @details
#' median takes parameters (points)
#'
#'
#' @return smoothed vector
#' @export
#'
#' @examples
smooth_vector <- function(x, type, ...){
  if(type == "median") {
    return(smooth_median(x, ...))
  }
  if(type == "spline") {
    return(smooth_spline(x, ...))
  }
}

smooth_median <- function(x, points = 11){
  if(any(is.na(x))){
    warning("There are NAs in the vector, replacing with last known value")
    x <- replace_na(x, "last.known")
  }
  x <- runmed(x, points, endrule = "constant")
  return(x)
}

smooth_approx <- function(x){
  x  <- approx(x)
  return(x)
}

smooth_spline <- function(x, spar = NULL, nknots = .nknots.smspl){
  x <- smooth.spline(x, spar = spar, nknots = nknots)$y
  return(x)
}
