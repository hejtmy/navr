#' Title
#'
#' @param timestamps timestamps of the distance measurements
#' @param distances distance measures in any unit
#'
#' @return vector of immediate speeds in given unit per second
#' @export
#'
#' @examples
calculate_speeds <- function(distances,timestamps){
  time_diffs <- navr::calculate_time_diffs(timestamps, NA)
  speeds <- distances/time_diffs
  return(speeds)
}
