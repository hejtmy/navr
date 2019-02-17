#' Adds new colum angle_diff_axis where it calculates angle difference between rows
#' @param df_log data frame log that gets modified
#' @param rotation
#' @param name what should be appended to "angle_diff_"
#'
#' @export
#'
#' @example
#' add_angle_difference(player_log, player_log$Rotation.x, "x")
add_angle_difference <- function(df_log, rotation, name){
  new_col_name <- paste0("angle_diff_", name)

  angle_diffs <- round(c(0, diff(rotation)), 4)
  angle_diffs <- angle_to_180(angle_diffs)

  df_log[, new_col_name] <- angle_diffs
  return(df_log)
}

#' Adds tiems since start column to the navr object
#'
#' @param obj object of appropriate class
#' @param ...
#'
#' @return object with its
#' @export
#'
#' @examples
add_times_since_start <- function(obj, ...){
  UseMethod("add_times_since_start")
}
#' @export
add_times_since_start.navr <- function(obj){
  obj$data$time_since_start <- navr::calculate_times_since_start(obj$data$timestamp)
  return(obj)
}

#' Adds distance and cumulative distances (distance_total) columns
#'
#' @param obj valid navr object
#' @param ...
#'
#' @return navr object
#' @export
#'
#' @examples
add_distances <- function(obj, ...){
  UseMethod("add_distances")
}
#' @export
add_distances.navr <- function(obj){
  mat_xy <- cbind(obj$data$position_x, obj$data$position_y)
  obj$data$distance <- navr::calculate_distances(mat_xy, 0)
  obj$data$distance_total <- cumsum(obj$data$distance)
  return(obj)
}
