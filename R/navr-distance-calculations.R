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
#' > print(euclid_distance(c(0,0), c(1,1)))
#' [1] 1.414214
euclid_distance <- function(point1, point2){
  # VALIDATE
  return(sqrt(sum((point2 - point1)^2)))
}

#' Eucilidan distance covered between each row
#'
#' @description
#' Calculates distance between points in each row
#'
#' @param points m x 2 matrix with first column being X and second designating Y coordinates
#'
#' @return vector of distances of length m (prepends 0 to the beginning)
#' @export
#'
#' @examples
#'
euclid_distance_between_rows <- function(points){
  points_shifted <- rbind(c(0,0), points[1:nrow(points) - 1, ])
  points_sub <- points - points_shifted
  vec_sums <- apply(points_sub, 1, function(x) sqrt(sum(x ^ 2)))
  return(vec_sums)
}
