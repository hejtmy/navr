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

#' Euclidian distance between two points
#' @description
#' Calculates euclidian distance between two points.
#'
#' @param point1 2D vector of X and Y position
#' @param point2 2D vector of X and Y position
#'
#' @return numeric or NA if not valid
#' @export
#'
#' @examples
#' print(euclid_distance(c(0,0), c(1,1)))
euclid_distance <- function(point1, point2){
  # VALIDATE
  return(sqrt(sum((point2 - point1)^2)))
}

#' Eucilidan distance covered between each row
#'
#' @description
#' Calculates distance between points in each row
#'
#' @param points m x 2 matrix or data.frame with first column X and second Y coordinates
#'
#' @return vector of distances of length m - 1
#'
#' @examples
euclid_distance_between_rows <- function(points){
  #first distance shoudl be rezo, that's why we copy the first row in the shifted matrix
  points_shifted <- rbind(points[1,], points[1:nrow(points) - 1, ])
  points_sub <- points - points_shifted
  vec_sums <- apply(points_sub, 1, function(x) sqrt(sum(x ^ 2)))
  vec_sums <- vec_sums[-1] #removing the first element
  return(vec_sums)
}


has_column <- function(df, column_name){
  return(column_name %in% colnames(df))
}
