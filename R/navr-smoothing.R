#' Smooths positions of a given boject
#'
#' @param obj valid object to be smoothed
#' @param type string: median, approx, spline
#' @param .... optional parameters for smoothing
#'
#' @return
#' @export
#'
#' @examples
smooth_positions <- function(obj, type, ...){
  UseMethod("smooth_positions")
}
#' @describeIn smooth_positions position smoothing in the navr object
#' @export
smooth_positions.navr <- function(obj, type, ....){
  if(is.null(ls)) return(NULL)
  obj$data$position_x <- smooth_vector(obj$data$position_x, type, ...)
  obj$data$position_y <- smooth_vector(obj$data$position_y, type, ...)
  return(obj)
}

#' Smooths speed
#'
#' @description Smooths speed values with given type of smoothing. IMPORTANT! Does not change
#' any positioning or other data, only smooths the speed values.
#' Speed smothing is generally important for clear onset search
#'
#' @param obj object to perfrom speed smoothing on
#' @param type type of smoothing. See \code{\link{smooth_vector}} vector for full description
#' @param ... optional parameters for given smoothing, see  \code{\link{smooth_vector}} function
#'
#' @return
#' @export
#'
#' @examples
smooth_speed <- function(obj, type, ...){
  UseMethod("smooth_speed")
}

#' @describeIn smooth_speed Smooths navr object speeds
#' @export
smooth_speed.navr <- function(obj, type, ...){
  obj$data$speed <- smooth_vector(obj$data$speed, type, ...)
  return(obj)
}

#' Smooths positions of given vector
#'
#' @param x numeric vector to smooth
#' @param type median, approx, spline
#' @param ... optional parameters for the smoothing function (see below)
#'
#' @details
#' *median* takes parameters points (default is 11). See \code{\link{runmed}}
#' *spline* takes parameters and spar, nknots. See \code{\link{smooth.spline}}
#'
#' @return smoothed numeric vector
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
