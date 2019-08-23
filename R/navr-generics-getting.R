#' Filters navr object to only include times in between
#'
#' @param obj
#' @param times a vector of length 2, start time and end time in seconds
#' @param zero_based signifying if you passed seconds from start or reaů time. defualts to False
#' @param ... aditional arguments
#'
#' @return
#' @export
#'
#' @examples
filter_times <- function(obj, times, zero_based, ...){
  UseMethod("filter_times")
}

#' @export
filter_times.navr <- function(obj, times, zero_based = F){
  if (zero_based){
    timesincestart <- get_times_since_start(obj) #allows stopping the function in case it hasn't been preprocessed
    i <- (timesincestart >= times[1]) & (timesincestart <= times[2])
    obj$data <- obj$data[i, ]
  } else {
    obj$data <- obj$data[obj$data$timestamp >= times[1] & obj$data$timestamp <= times[2], ]
  }
  return(obj)
}

#' Return time since start form the passed object
#'
#' @param obj
#'
#' @return
#' @export
get_times_since_start <- function(obj, ...){
  UseMethod('get_times_since_start')
}
#' Returns time since start column from the navr object if it has one
#'
#' @param obj Navr Object
#'
#' @return
#' @export
#'
#' @examples
get_times_since_start.navr <- function(obj){
  if(!is.null(obj$data$time_since_start)) return(obj$data$time_since_start)
  stop("The object doesn't have time_since_start column. Have you ran *add_times_since_start* on the object?")
}

#' Return time diffs column from the object
#'
#' @param obj
#'
#' @return
#' @export
get_time_diffs <- function(obj, ...){
  UseMethod('get_time_diffs')
}

#' Returns time_diffs column from the data if it has been caluclated
#'
#' @param obj Navr Object
#'
#' @return
#' @export
#'
#' @examples
get_time_diffs.navr <- function(obj){
  if(!is.null(obj$data$time_diff)) return(obj$data$time_diff)
  stop("The object doesn't have time_diff column. Have you ran *add_time_diffs* on the object?")
}

#' Gets distance column from the object
#' @param obj navr object with calculated distances
#'
#' @param ...
#'
#' @export
get_distances <- function(obj, ...){
  UseMethod("get_distances")
}
#' @export
get_distances.navr <- function(obj){
  if(!is.null(obj$data$distance)) return(obj$data$distance)
  stop("The object doesn't have distances column. Have you ran *add_distances* on the object?")
}

#' Gets speed column from the object
#' @param obj navr object with calculated speeds
#'
#' @param ...
#'
#' @export
get_speeds <- function(obj, ...){
  UseMethod("get_speeds")
}

#' @export
get_speeds.navr <- function(obj){
  if(!is.null(obj$data$speed)) return(obj$data$speed)
  stop("The object doesn't have speed column. Have you ran *add_speeds* on the object?")
}
