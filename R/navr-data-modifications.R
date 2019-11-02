#' Adds all necessary columns to the navr object
#'
#' @param obj object of type navr
#'
#' @return returns navr object with modified columns
#' @export
#'
#' @examples
prepare_navr <- function(obj) {
  obj <- add_time_columns(obj)
  obj <- add_distances(obj)
  obj <- add_speeds(obj)
  obj <- add_angle_differences(obj)
  return(obj)
}

#' Adds all necessary columns to the navr object
#'
#' @description runs add_time_columns, add_distances, add_angle_differences and add_speeds
#'
#' @param obj object of type navr
#'
#' @return returns navr object with modified columns
#' @export
#'
#' @examples
add_columns_navr <- function(obj){
  .Deprecated("prepare_navr")
  return(prepare_navr(obj))
}

#' Add time_since_start and time_diff columns
#'
#' @param obj Navr Object
#' @param ...
#' @export
add_time_columns <- function(obj, ...){
  .Deprecated("prepare_navr")
  UseMethod('add_time_columns')
}
#' @export
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
  .Deprecated("prepare_navr")
  UseMethod("add_times_since_start")
}
#' @export
add_times_since_start.navr <- function(obj){
  obj$data$time_since_start <- calculate_times_since_start(obj$data$timestamp)
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
  .Deprecated("prepare_navr")
  UseMethod("add_time_diffs")
}
#' @export
add_time_diffs.navr <- function(obj){
  obj$data$time_diff <- calculate_time_diffs(obj$data$timestamp, first_value = 0)
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
  .Deprecated("prepare_navr")
  UseMethod("add_distances")
}
#' @export
add_distances.navr <- function(obj){
  mat_xy <- cbind(obj$data$position_x, obj$data$position_y)
  obj$data$distance <- navr::calculate_distances(mat_xy, 0)
  obj$data$distance_total <- calculate_total_distance(obj$data$distance)
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
  .Deprecated("prepare_navr")
  cols <- colnames(obj$data)
  for(i in grep("rotation", cols)){
    colname <- cols[i]
    new_name <- paste0(colname, "_diff") #appends
    obj$data[[new_name]] <- calculate_angle_differences(obj$data[[colname]])
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
  .Deprecated("prepare_navr")
  UseMethod("add_speeds")
}
#' @export
add_speeds.navr <- function(obj){
  distances <- get_distances.navr(obj)
  obj$data$speed <- navr::calculate_speeds(distances, obj$data$timestamp)
  return(obj)
}

#' Shorthand for picking speeds and then recalculating object
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_unreal_speeds <- function(obj, ...){
  UseMethod("remove_unreal_speeds")
}

#' Shorthand for picking speeds and then recalculating object
#'
#' @description Picks unreal speeds in the same way as `pick_unreal_speed` but then removes
#' given lines form the data, replaces them as per replacement parameter and returnes cleaned
#' object
#' @param cutoff cutoff value. see `pick_unreal_speeds`
#' @param type what type of cutoof. see `pick_unreal_speeds`
#' @param remove_distance shoudl the distances be removed? in case unreal speeds are probably due to "teleportation" or bad measurements,
#' jumpy tracking etc,  then the participant didn't really travel that distance and we can remove it. In case the unreal distances are caused
#' by parts of the tracking missing, then the distances travelled are actually correct, just the speeds are not. *Defaults* to TRUE
#' @param total_recalculate if true, recalculates total_distance column to reflect removed distances, *defaults* to TRUE
#' @param replacement what to replace  unreal speeds with. *Defaults* to NA
#' @param indices indices of speeds to clean out in case they were obtained separately.
#' If empty, `type`, `cutoff` need to be defined and `pick_unreal_speeds` is called
#'
#' @export
remove_unreal_speeds.navr <- function(obj, cutoff = NULL, type = NULL,
                                      remove_distance = T, total_recalculate = T,
                                      indices = c(), replacement = NA){
  if(!("speed" %in% colnames(obj$data))){
    warning("The object doesn't have speed column. You need to add it first using add_speeds function")
    return(obj)
  }
  if(length(indices) == 0){
    if(any(is.null(cutoff), is.null(type))){
      stop("Indices not provided and cutoff and type are not defined. Cannot pick_unreal_speeds.")
    }
    indices <- pick_unreal_speeds(obj, cutoff, type)
    if(is.null(indices)) return(obj)
  }
  obj$data[indices, "speed"] <- replacement
  if(remove_distance) obj$data[indices, "distance"] <- NA
  if(total_recalculate) obj$data$distance_total <- calculate_total_distance(obj$data$distance)
  return(obj)
}
