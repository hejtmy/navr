#' Gets log data.frame for a particular trial
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
#' @return list with start and finish field
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

#' Gets log between determined times
#'
#' @param obj
#' @param start start time as it appears in the log
#' @param end end time as it appears in the log
#' @param ...
#'
#' @return data.frame
#' @export
#'
#' @examples
get_log_timewindow <- function(obj, start, end, ...){
  UseMethod("get_log_timewindow")
}

#' Gets log
#'
#' @param obj
#' @param ...
#'
#' @return data.frame
#' @export
#'
#' @examples
get_log <- function(obj, ...){
  UseMethod("get_log")
}

