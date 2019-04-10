#' Calcualtes vector of doubles with speeds
#'
#' @param timestamps timestamps of the distance measurements
#' @param distances distance measures in any unit
#'
#' @return vector of immediate speeds in given unit per second
#' @export
#'
#' @examples
calculate_speeds <- function(distances, timestamps){
  time_diffs <- navr::calculate_time_diffs(timestamps, NA)
  speeds <- distances/time_diffs
  speeds[is.infinite(abs(speeds))] <- NA
  return(speeds)
}

#' Finds indices of impossible speed values
#'
#' @param speeds speed vector of doubles
#' @param cutoff value after which we should consider speeds as invalid
#' @param type what type of cutoff. Possibilities value (above value is unreal),
#' percent (move than certain percent increase), std (number of stds away from mean speed),
#' quantile(percent of highest values)
#'
#' @return indices of speeds considered unreal
#' @export
#'
#' @examples
pick_unreal_speeds <- function(obj, cutoff, type="value"){
  UseMethod('pick_unreal_speeds')
}

#' @export
pick_unreal_speeds.navr <- function(obj, cutoff, type="value"){
  #needs to check of the speed even exists as a column
  if(!("speed" %in% colnames(obj$data))){
    warning("The object doesn't have speed column. You need to add it first using add_speeds function")
    return(NULL)
  }
  return(pick_unreal_speeds(obj$data$speed, cutoff, type))
}

#' @export
pick_unreal_speeds.double <- function(speeds, cutoff, type="value"){
  if(type=="value"){
    return(which(speeds > cutoff))
  }
  if(type=="std"){
    scaled_speeds <- scale(speeds)
    return(which(scaled_speeds > cutoff))
  }
  if(type=="quantile"){

  }
}
