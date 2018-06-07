#' Gets log data.frame for a particular trial
#'
#' @param obj
#' @param trialId integer with valid trialId
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
#' @param trialId integer with valid trialId
#' @param ...
#'
#' @return list with start and finish field
#' @export
#'
#' @examples
get_trial_times <- function(obj, trialId, ...){
  UseMethod("get_trial_times")
}


#' Returns duration of the trial in s
#'
#' @param obj
#' @param trialId
#' @param ...
#'
#' @return time in s
#' @export
#'
#' @examples
get_trial_duration <- function(obj, trialId, ...){
  UseMethod("get_trial_duration")
}

#' REturns disntace traveled during a particular trial
#'
#' @param obj
#' @param trialId
#' @param ...
#'
#' @return distance in appropriate units
#' @export
#'
#' @examples
get_trial_distance <- function(obj, trialId, ...){
  UseMethod("get_trial_distance")
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

#' Returns times when certain action was performend
#'
#' @param obj
#' @param action string with the action name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_action_times <- function(obj, action, ...){
  UseMethod("get_action_times")
}

#' Returns times when certain action was performend during a particular trial
#'
#' @param obj
#' @param trialId integer with valid trialId
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

#' Returns how many actions of particular type were recorded
#'
#' @param obj
#' @param action string with searched action name
#' @param ... extra parameters
#'
#' @return integer with number of recorded actions
#' @export
#'
#' @examples
get_n_actions <- function(obj, action, ...){
  UseMethod("get_n_actions")
}

#' Returns how many actions of particular type were recorded during particular trial
#'
#' @param obj
#' @param trialId integer with valid trialId
#' @param action string with searched action name
#' @param ...
#'
#' @return integer with number of recorded actions during particular trial
#' @export
#'
#' @examples
get_trial_n_actions <- function(obj, trialId, action, ...){
  UseMethod("get_trial_n_actions")
}
