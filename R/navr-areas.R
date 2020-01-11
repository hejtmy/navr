AREA_COLNAME <- "area"
#' Definition of an area object used in designating area presence
#'
#' @param name character. Name of the area
#' @param type character. defining what type of area. Currently supported is: "rectangle"
#' @param points numeric matrix (npoints, 2) with each row representing a single point and
#' columns representing X and Y positions
#'
#' @return
#' @export
#'
#' @examples
#' AreaObject("main square", "rectangle", matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol=2, byrow=T))
AreaObject <- function(name, type = "rectangle", points = c()){
  res <- list(name = name, type = type, points = points)
  # add validations
  class(res) <- append(class(res), "AreaObject")
  return(res)
}

#' Adds information about what area does the current position belong to
#'
#' @description The column contains information about in which area the
#'
#' @param obj
#' @param areas list of \coode{\link{AreaObject}}. Needs to be a list of length(areas), even if you are passing
#' a single area, you should pass it as a list(your_area)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_areas <- function(obj, areas, ...){
  UseMethod("add_areas")
}
#' @export
add_areas.navr <- function(obj, areas, ...){
  obj$data[[AREA_COLNAME]] <- NA_character_
  for(area in areas){
    iPresent <- is_in_area(obj$data$position_x, obj$data$position_y, area)
    obj$data[iPresent, AREA_COLNAME] <- area$name
  }
  return(obj)
}

#' Returns if the areas have already been calculated on the object
#'
#' @param obj valid navr object
#'
#' @return logical
#' @export
#'
#' @examples
has_areas <- function(obj){
  UseMethod("has_areas")
}

#' @export
has_areas.navr <- function(obj){
  return(AREA_COLNAME %in% colnames(obj$data))
}


#' Returns if given position is within area or not
#'
#' @param x numeric vector of x positions. Same dimensions as y
#' @param y numeric vector of y positions. Same dimensions as x
#' @param area \code{\link{AreaObject}}
#'
#' @return logical vector of length x.
#' @export
#'
#' @examples
is_in_area <- function(x, y, area){
  if(area$type == "rectangle") return(is_in_area.rectangle(x, y, area$points))
  warning("area's type ", area$type, " is not valid")
  return(FALSE)
}

is_in_area.rectangle <- function(x, y, points){
  xmin <- min(points[, 1])
  xmax <- max(points[, 1])
  ymin <- min(points[, 2])
  ymax <- max(points[, 2])
  return((x >= xmin & x <= xmax) & (y >= ymin & y <= ymax))
}

## Visualisatons ------

#' Returns area ploygon to be plotted
#'
#' @param area AreaObject
#' @param ... other ggplot arguments for geom_polygon
#'
#' @return ggplot geom_polygon object
#' @export
#'
#' @examples
geom_navr_area <- function(area, fill = NA, color = "red", size = 1.25, ...){
  df <- as.data.frame(area$points)
  colnames(df) <- c("x", "y")
  return(geom_polygon(data = df, aes(x, y), fill = fill,
                      color = color, size = size, ...))
}
