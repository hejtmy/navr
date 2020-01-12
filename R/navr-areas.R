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

## Analysis -----
#' Calculates time spent in each of encoded areas
#'
#' @param obj preprocessed navr object with added areas (see \code{\link{add_areas}})
#'
#' @return dataframe with results
#' @export
#'
#' @examples
calculate_areas_time <- function(obj){
  if(!has_areas(obj)){
    warning("Areas have not been added yet. Have you run add_areas?")
    return(NULL)
  }
  dat <- obj$data
  total_time <- tail(dat$timestamp, 1) - dat$timestamp[1]
  df <- data.frame(area = "anywhere",
                   n = nrow(dat),
                   duration = total_time,
                   ratio = 1,
                   stringsAsFactors = FALSE)
  areas <- unique(dat[[AREA_COLNAME]])
  areas <- areas[!is.na(areas)]
  for(area in areas){
    area_presence <- get_area_position(obj, area)
    area_presence <- area_presence$data
    ls <- list(area = area)
    ls$n <- nrow(area_presence)
    ls$duration <- sum(area_presence$time_diff, na.rm = TRUE)
    ls$ratio <- ls$duration/total_time
    df <- rbind(df, ls)
  }
  return(df)
}

## Getters ----

#' Returns only data with area
#'
#' @param obj
#' @param area
#'
#' @return
#' @export
#'
#' @examples
get_area_position <- function(obj, area){
  if(!has_areas(obj)){
    warning("Areas have not been added yet. Have you run add_areas?")
    return(NULL)
  }
  obj$data <- obj$data[obj$data[[AREA_COLNAME]] == area & !is.na(obj$data[[AREA_COLNAME]]), ]
  return(obj)
}


#' Returns indices of when an area was entered for the first time
#'
#' @param obj navr object with areas.
#' @param to name of the area to which count entrances
#' @param from optional parameter defining from whih area the entrance should be
#' @param between_allowed number of allowed areas to be entered between the `from` and `to`.
#' only aplicable if `from` is set. Designates if  the entrance
#' should be counted only if it is N "stops from the `from` area or if any
#' This is primarily for the reason of defining "no" enter zones or if you have some "buffer" between.
#' e.g. if you want to count how many time supermarket is entered from
#' the "street", but the person has to pass through a "parking lot" area first, you need to
#' set the `between_allowed` to 1 or higher.
#'
#' @return indices of data where the area was entered given argument conditions
#' @export
#'
#' @examples
get_area_visits <- function(obj, to, from = NULL, between_allowed = 0){
  if(!has_areas(obj)){
    warning("Areas have not been added yet. Have you run add_areas?")
    return(NULL)
  }
  areas <- obj$data[[AREA_COLNAME]]
  areas[is.na(areas)] <- "_nowhere_"
  areas_visited <- rle(areas)
  iVisits <- which(areas_visited$values == to)
  # if the test starts there, we don¨t consider it as a visit
  iVisits <- iVisits[iVisits != 0]
  ## Dealing witht he from argument
  if (!is.null(from)){
    iKeep <- c()
    iBeforeStarts <- iVisits - (between_allowed + 1) #always at least one back
    iBeforeStarts[iBeforeStarts < 1] <- 1
    for(i in 1:length(iVisits)){
      # all the areas/indices of locations visited before this particular visit
      iBetween <- iBeforeStarts[i]:(iVisits[i] - 1)
      # if the area vas visited within the iBetwen we have to start calculating from the last visit
      if(to %in% areas_visited$values[iBetween]){
        iBeforeStart <- max(iBetween[areas_visited$values[iBetween] == to])
        iBetween <- iBeforeStart:(iVisits[i] - 1)
      }
      if(from %in% areas_visited$values[iBetween]) iKeep <- c(iKeep, i)
    }
    iVisits <- iVisits[iKeep]
  }
  indices <- sapply(iVisits, function(x){sum(areas_visited$lengths[1:(x - 1)])})
  if(length(indices) == 0) indices <- c()
  return(indices)
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
