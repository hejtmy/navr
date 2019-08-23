#' Searches for times when the movement onset started
#'
#' @param obj
#' @param speed_threshold
#' @param min_duration
#' @param return_duration
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
search_onsets <- function(obj, speed_threshold, min_duration, return_duration, ...){
  UseMethod("search_onsets")
}

#' Title
#'
#' @param obj navr object with calculated speeds
#' @param speed_threshold
#' @param min_duration
#' @param return_duration
#' @param still_speed_threshold
#' @param still_duration
#'
#' @return
#' @export
#'
#' @examples
search_onsets.navr <- function(obj, speed_threshold, min_duration = 0, return_duration = F,
                               still_speed_threshold = speed_threshold, still_duration = 0){
  speeds <- get
}

search_onsets.double <- function(speeds){
  #return indices
}
