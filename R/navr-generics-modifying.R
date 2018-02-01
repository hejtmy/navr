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

#' Adds goal order to the object
#'
#' @param obj
#' @param order vector with goal order. Indexed from 1 to n:rows of goal positions
#' @param ... aditional arguments
#'
#' @return modified object
#' @export
#'
#' @examples
add_goal_order <- function(obj, order, ...){
  UseMethod("add_goal_order")
}

