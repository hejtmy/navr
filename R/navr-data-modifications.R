#' Adds new colum angle_diff_axis where it calculates angle difference between rows
#' @param df_log data frame log that gets modified
#' @param rotation
#' @param name what should be appended to "angle_diff_"
#'
#' @export
#'
#' @example
#' add_angle_difference(player_log, player_log$Rotation.x, "x")
#'
add_angle_difference <- function(df_log, rotation, name){
  new_col_name <- paste0("angle_diff_", name)

  angle_diffs <- c(0, diff(rotation))
  angle_diffs <- angle_to_180(axis_angle_diffs)

  player_log$new_col_name <- axis_angle_diffs
  return(player_log)
}
