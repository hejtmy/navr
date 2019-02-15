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
    timesincestart <- get_times_since_start(obj)
  } else {
    obj$data <- obj$data[obj$data$timestamp > times[1] & obj$data$timestamp < times[2], ]
  }
  return(obj)
}

get_times_since_start <- function(obj, ...){

}
