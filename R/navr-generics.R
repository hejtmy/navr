#' Generic function
#'
#' @param obj
#' @param trialId vector of trial ids
#' @param ... Optional parameters
#'
#' @return ggplot object
#' @export
plot_trial_path <- function (obj, trialId, ...){
  UseMethod("plot_trial_path")
}

#' Adds goal positions to the object
#'
#' @param obj
#' @param df data frame with goal positions with appropriate column naming
#' @param ...
#'
#' @return modified object
#' @export
#'
#' @examples
add_goal_positions <- function(obj, df, ...){
  UseMethod("add_goal_positions")
}
