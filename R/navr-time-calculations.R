#' Calculates time differences between timestamps
#'
#' @param timestamps vector of timestamps in seconds
#' @param first_value what to insert in the first position (default NULL)
#'
#' @return vector of size of timestamps-1 or timestamps in case first value is set
#' @export
#'
#' @examples
calculate_time_diffs <- function(timestamps, first_value = NULL){
  time_diffs <- diff(timestamps)
  if(!is.null(first_value)) time_diffs <- c(first_value, time_diffs)
  return(time_diffs)
}

#' Calculates times since start
#'
#' @param timestamps timestamps in seconds
#'
#' @return vector of size of timestamps (startign with 0)
#' @export
#'
#' @examples
calculate_times_since_start <- function(timestamps){
  time_diffs <- diff(timestamps)
  time_diffs <- c(0, time_diffs)
  times_since_start <- cumsum(time_diffs)
  return(times_since_start)
}
