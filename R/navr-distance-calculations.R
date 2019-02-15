#' Calculates distances froma x and y vectors
#'
#' @param positions_x vector of consecutive x positions
#' @param positions_y vector of consecutive y positions
#' @param first_value value to be appended to the first position (typically NA, or 0), if NULL, nothing is appended
#'
#' @return vector of distances of length x-1 or x if first value is defined
#' @export
#'
#' @examples
calculate_distances <- function(mat_xy, first_value = NULL){
  distances <- euclid_distance_between_rows(mat_xy)
  if(!is.null(first_value)) distances <- c(first_value, distances)
  return(distances)
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
#' @param points m x 2 matrix or data.frame with first column X and second Y coordinates
#'
#' @return vector of distances of length m (prepends 0 to the beginning)
#'
#' @examples
#'
euclid_distance_between_rows <- function(points){
  #first distance shoudl be rezo, that's why we copy the first row in the shifted matrix
  points_shifted <- rbind(points[1,], points[1:nrow(points) - 1, ])
  points_sub <- points - points_shifted
  vec_sums <- apply(points_sub, 1, function(x) sqrt(sum(x ^ 2)))
  return(vec_sums)
}
