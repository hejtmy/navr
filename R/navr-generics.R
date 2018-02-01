#' Generic function
#'
#' @param trialId
#'
#' @return ggplot object
#' @export
plot_trial_path <- function (obj, trialId, ...){
  UseMethod("plot_trial_path")
}
