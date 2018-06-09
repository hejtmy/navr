#' Smooths positions in a data frame
#'
#' @param df Data frame to modify
#' @param col_x name of position of the df column with X variable
#' @param col_y name of position of the df column with Y variable
#' @param type median, approx, spline
#'
#' @return
#' @export
#'
#' @examples
smooth_positions_df <- function(df, col_x, col_y, type, ...){
  if(type == "median"){
    ls <- smooth_positions_df_median(df[, (col_x)], df[, (col_y)], ...)
  }
  if(type == "spline"){
    ls <- smooth_positions_df_spline(df[, (col_x)], df[, (col_y)], ...)
  }
  df[, (col_x)] <- ls$x
  df[, (col_y)] <- ls$y
  return(df)
}

smooth_positions_df_median <- function(x, y, points = 11){
  x <- runmed(x, points, endrule = "constant")
  y <- runmed(y, points, endrule = "constant")
  return(list(x = x, y = y))
}

smooth_positions_df_approx <- function(x, y){
  x  <- approx(x)
  y <- approx(y)
  return(list(x = x, y = y))
}

smooth_positions_df_spline <- function(x, y, spar = NULL, nknots = .nknots.smspl){
  x <- smooth.spline(x, spar = spar, nknots = nknots)$y
  y <- smooth.spline(y, spar = spar, nknots = nknots)$y
  return(list(x = x, y = y))
}
