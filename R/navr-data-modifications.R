#' Add time_since_start and time_diff columns
#'
#' @param obj Navr Object
#' @param ...
#' @export
add_time_columns <- function(obj, ...){
  UseMethod('add_time_columns')
}
#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
add_time_columns.navr <- function(obj){
  obj <- add_times_since_start(obj)
  obj <- add_time_diffs(obj)
  return(obj)
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

#' Adds tiems since start column to the navr object
#'
#' @param obj object of appropriate class
#' @param ...
#'
#' @return object with its
#' @export
#'
#' @examples
add_time_diffs <- function(obj, ...){
  UseMethod("add_time_diffs")
}
#' @export
add_time_diffs.navr <- function(obj){
  obj$data$time_diff <- navr::calculate_time_diffs(obj$data$timestamp, first_value = 0)
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
  obj$data$distance_total <- cumsum(obj$data$distance[!is.na(obj$data$distance)])
  return(obj)
}

#' Adds extra diff column to each column that has "rotation" in its name
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
add_angle_differences <- function(obj){
  cols <- colnames(obj$data)
  for(i in grep("rotation", cols)){
    colname <- cols[i]
    new_name <- paste0(colname, "_diff") #appends
    obj$data[[new_name]] <- navr::calculate_angle_differences(obj$data[[colname]])
  }
  return(obj)
}

#' Adds speed column
#'
#' @param obj valid navr object
#' @param ...
#'
#' @return navr object
#' @export
#'
#' @examples
add_speeds <- function(obj, ...){
  UseMethod("add_speeds")
}
#' @export
add_speeds.navr <- function(obj){
  distances <- get_distances(obj)
  obj$data$speed <- navr::calculate_speeds(distances, obj$data$timestamp)
  return(obj)
}

#' Inserts NA values to speed and distance
#'
#' @param obj
#' @param indices indices of speeds to clean out
#' @param total_recalculate if true, recalculates total_distance column to reflect removed distances
#'
#' @return navr object with NA values in appropriate places
#' @export
#'
#' @examples
remove_unreal_speeds <- function(obj, indices, total_recalculate = T){
  UseMethod("remove_unreal_speeds")
}
#' @export
remove_unreal_speeds.navr <- function(obj, indices, total_recalculate = T){
  obj$data[indices, c("distance", "speed")] <- c(NA, NA)
  obj$data$distance_total <- cumsum(obj$data$distance[!is.na(obj$data$distance)])
  return(obj)
}
