#' Filters navr object to only include times in between
#'
#' @param obj object to filter
#' @param times a vector of length 2, or a matrix with two columns, start time and end time in seconds
#' @param zero_based signifying if you passed seconds from start or real time. defualts to FALSE (real time passed)
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
  # Do some validations of the passed times
  times <- matrix(times, ncol = 2) #tehcnically leaves times unchanged for the vector
  search_times <- obj$data$timestamp
  if (zero_based) search_times <- get_times_since_start.navr(obj) #allows stopping the function in case it hasn't been preprocessed
  mat_filt <- matrix(FALSE, ncol = nrow(times), nrow=length(search_times))
  for(i in 1:nrow(times)){
    i_fit <- (search_times >= times[i,1]) & (search_times <= times[i,2])
    mat_filt[,i] <- i_fit
  }
  i_fit <- apply(mat_filt, 1, any) #is there any TRUE in the mat_filt
  obj$data <- obj$data[i_fit, ]
  return(obj)
}

#' Returns time since start column from the navr object if it has one
#'
#' @param obj Navr Object
#'
#' @return
get_times_since_start.navr <- function(obj){
  if(!is.null(obj$data$time_since_start)) return(obj$data$time_since_start)
  stop("The object doesn't have time_since_start column. Did you run *prepare_navr* on the object?")
}

#' Returns time_diffs column from the data if it has been caluclated
#'
#' @param obj Navr Object
#'
#' @return
get_time_diffs.navr <- function(obj){
  if(!is.null(obj$data$time_diff)) return(obj$data$time_diff)
  stop("The object doesn't have time_diff column. Have you ran *add_time_diffs* on the object?")
}

#' Gets distance column from the object
#' @param obj navr object with calculated distances
#'
#' @param ...
get_distances.navr <- function(obj){
  if(!is.null(obj$data$distance)) return(obj$data$distance)
  stop("The object doesn't have distances column. Have you ran *add_distances* on the object?")
}

#' Gets speed column from the object
#' @param obj navr object with calculated speeds
#'
get_speeds.navr <- function(obj){
  if(!is.null(obj$data$speed)) return(obj$data$speed)
  stop("The object doesn't have speed column. Have you ran *add_speeds* on the object?")
}
