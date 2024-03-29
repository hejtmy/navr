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
  rolling <- rowSums(outer(1:(length(vec) - n_points+1), 1:n_points,
                           FUN = function(i,j){vec[(j - 1) + i]}))
  return(rolling)
}

#' Euclidian distance between two points
#' @description
#' Calculates euclidian distance between two points.
#'
#' @param point1 numeric position (usually length 2 or 3)
#' @param point2 numeric position (usually length 2 or 3)
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

#' Replaces na values with given type
#'
#' @param vec vector to replace numbers with
#' @param replacement: "last.known", "number"
#' @param ...
#'
#' @noRd
replace_na <- function(vec, replacement = "last.known", ...){
  # neat hack from https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
  if(replacement == "last.known"){
    if(is.na(vec[1])){
      vec[1] <- vec[!is.na(vec)][1] #needs to replace the first NA
    }
    vec <- na.omit(vec)[cumsum(!is.na(vec))]
    return(vec)
  }
}

has_column <- function(df, column_name){
  return(column_name %in% colnames(df))
}
