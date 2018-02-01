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

