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


#' Title
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_times_since_start <- function(obj, ...){
  UseMethod('get_times_since_start')
}

#' Title
#'
#' @param obj Navr Object
#'
#' @return
#' @export
#'
#' @examples
get_times_since_start.navr <- function(obj){
  if(!is.null(obj$data$time_since_start)) return(obj$data$time_since_start)
  stop("The object doesn't have time_since_start column. Have you ran *add_times_since_start*?")
}
