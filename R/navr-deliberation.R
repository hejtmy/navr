#' Searches for deliberation stop times and durations (time of no movement and increased rotation)
#'
#' @description if min_rotation is 0, then it can be replaced by search stops.
#'
#' @param obj
#' @param speed_threshold what is considered as a stopping speed. Same as the speed threshold in search_stops
#' @param min_duration minimal duration of the stop
#' @param min_rotation
#' @param rotation_axis
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
search_deliberation_stops <- function(obj, speed_threshold, min_duration, min_rotation, rotation_axis = "x", ...){
  UseMethod("search_deliberation_stops")
}

#' @describeIn search_deliberation_stops Searches for deliberation stops in navr object
#' @export
search_deliberation_stops.navr <- function(obj, speed_threshold, min_duration, min_rotation, rotation_axis = "x", ...) {
  # checks if columns are present
  stops <- search_stops.navr(obj, speed_threshold, min_duration)
  if(min_rotation > 0) obj <- add_angle_differences(obj)
  rotation_column <- paste0("rotation_", rotation_axis, "_diff")
  times <- c()
  times_since_start <- c()
  durations <- c()
  for(i in 1:length(stops$time_since_start)){
    small <- filter_times.navr(obj, c(stops$time_since_start[i], stops$time_since_start[i]+stops$duration[i]), zero_based = TRUE)
    if(sum(small$data[[rotation_column]]) < min_rotation) next
    times <- c(times, stops$time[i])
    times_since_start <- c(times_since_start, stops$time_since_start[i])
    durations <- c(durations, stops$duration[i])
  }
  result <- list(time = times,
                 time_since_start = times_since_start,
                 duration = durations)
  return(result)
}
