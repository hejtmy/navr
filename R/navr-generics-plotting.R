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
