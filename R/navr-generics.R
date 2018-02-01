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
#' @param ... aditional arguments
#'
#' @return modified object
#' @export
#'
#' @examples
add_goal_positions <- function(obj, df, ...){
  UseMethod("add_goal_positions")
}

#' Title
#'
#' @param obj
#' @param trialId
#' @param ... aditional arguments
#'
#' @return
#' @export
#'
#' @examples
get_trial_log <- function(obj, trialId, ...){
  UseMethod("get_trial_log")
}

#' Returns list with trial times
#'
#' @param obj
#' @param trialId
#' @param ...
#'
#' @return list with start and end field
#' @export
#'
#' @examples
get_trial_times <- function(obj, trialId, ...){
  UseMethod("get_trial_times")
}

#' Returns times when certain action was performend during a particular trial
#'
#' @param obj
#' @param trialId
#' @param action string with the action name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_trial_action_times <- function(obj, trialId, action, ...){
  UseMethod("get_trial_action_times")
}

#' Title
#'
#' @param obj
#' @param trialId
#' @param ...
#'
#' @return vector 2 of x and Y of the goal position
#' @export
#'
#' @examples
get_trial_goal_position <- function(obj, trialId, ...){
  UseMethod("get_trial_goal_position")
}


#' Returns X and Y of a starting position
#'
#' @param obj
#' @param trialId
#' @param ...
#'
#' @return vector 2 of x and Y of the start position
#' @export
#'
#' @examples
get_trial_start_position <- function(obj, trialId, ...){
  UseMethod("get_trial_start_position")
}

