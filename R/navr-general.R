#' Calculates rolling sum of a vector across N points
#'
#' @param vec numeric vector across which we calculate
#' @param n_points
#'
#' @return rolled vector sum or NULL if the n_points is larger than length of the vector
#' @export
#'
#' @examples
rolling_sum <- function(vec, n_points){
  if(length(vec) < n_points) return(NULL)
  rolling <- rowSums(outer(1:(length(vec) - n_points+1), 1:n_points, FUN=function(i,j){vec[(j - 1) + i]}))
  return(rolling)
}
