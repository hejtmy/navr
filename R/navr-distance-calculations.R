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

calculate_total_distance <- function(distances){
  distances[is.na(distances)] <- 0
  return(cumsum(distances))
}
