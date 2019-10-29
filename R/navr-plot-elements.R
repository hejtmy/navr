#' Geom of plotting navr path
#'
#' @param obj NavrObject
#' @param add_points if points should be explicitely noted on the path
#' @param add_rotation if each point should also have a directional arrow attached
#' @param ... additional geom_path arguments
#'
#' @return
#' @export
#'
#' @examples
geom_navr_path <- function(obj, add_points = F, ...){
  df_position <- data.frame(x = obj$data$position_x, y = obj$data$position_y)
  ls <- list(geom_path(data = df_position, aes(x, y), ...))
  if(add_points) ls <- c(ls, geom_point(data = df_position, aes(x, y)))
  return(ls)
}

#' Adds rotational information to the path data
#'
#' @details plots arrows pointint in ditational direction. Uses `geom_navr_direction` function to plot each rotation
#'
#' @param obj navr object
#' @param axis character designating which rotation axis to plot. Needs to follow the navr specifications.
#' e.g. if "z", the obj needs to have "rotation_z" column
#' @param downsample For very frequent data, this function would return several thousands
#' geoms. Therefore it is recommended to only sample each nth observation.
#' @param length arrow length
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_path_rotation <- function(obj, axis = "x", downsample = 10, length = 1, ...){
  rot_name <- paste0("rotation_", axis)
  df <- data.frame(x = obj$data$position_x, y = obj$data$position_y, angle=obj$data[[rot_name]])
  df <- df[seq(1, nrow(df), downsample), ]
  ls <- c()
  for(i in 1:nrow(df)){
    position <- c(df$x[i], df$y[i])
    angle <- df$angle[i]
    ls <- c(ls, geom_navr_direction(position, angle, length = length, ...))
  }
  return(ls)
}

#' GGplot geom to add custom PNG background to the plot
#'
#' @param image_path path the png image
#' @param xlim background poistions as vector of length 2, c(-100,400)
#' @param ylim background poistions as vector of length 2, c(-100,400)
#'
#' @return
#' @export
#'
#' @examples
geom_navr_backround <- function(image_path, xlim = NULL, ylim = NULL){
  img <- png::readPNG(image_path)
  rast <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = T)
  if(!(is.null(xlim) | is.null(ylim))){
    #checks size of xlim and Ylim
    return(annotation_custom(rast, xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]))
  } else {
    return(annotation_custom(rast))
  }
}

#' GGplot geom to add specified points to the given plot
#'
#' @param ls list with XY vectors. eg. (list(start = c(0, 0), end = C(10, 5)))
#' @param ... ggplot additional params
#'
#' @return
#' @export
#'
#' @examples
geom_navr_points <- function(ls, ...){
  list_names <- names(ls)
  df <- data.frame(point_x = numeric(0), point_y = numeric(0), point_name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    df[i, 1] <- ls[[i]][1]
    df[i, 2] <- ls[[i]][2]
    df[i, 3] <- list_names[i]
  }
  ls <- list(geom_point(data = df, aes(point_x, point_y), ...),
              geom_text(data = df, aes(point_x, point_y, label = point_name)))
  return(ls)
}

#' Adds arrow pointing from a point in a specified angle
#'
#' @param position numeric(2) of X and Y position
#' @param angle numeric angle in degrees (0-360)
#' @param length length od the arrow to be drawm
#' @param ... additional ggplot values
#'
#' @return ggplot element
#' @export
#'
#' @examples geom_navr_direction(c(0,0), 180, 5, color = "red")
geom_navr_direction <- function(position, angle, length = 1, ...){
  ARROW_DEF <- arrow(length = unit(0.25, "cm"))
  arrow_line <- create_direction_line(position, angle, length)
  return(geom_segment(data = arrow_line,
                            aes(x = x, y = y, xend = xend, yend = yend),
                            arrow = ARROW_DEF, ... ))
}

#' Adds limits to the plot from the area_boundaries list field
#' @description for better control, just use regular xlim and ylim functions, this is just a shorthand
#'
#' @param obj Navr object with area_boundaries field
#'
#' @return list with xlim and ylim based on object area boundaries
#' @export
#'
#' @examples
geom_navr_limits <- function(obj){
  ls <- list()
  if(is.null(obj$area_boundaries)) return(ls)
  if(!is.null(obj$area_boundaries$x)) ls <- c(ls, xlim(obj$area_boundaries$x))
  if(!is.null(obj$area_boundaries$y)) ls <- c(ls, ylim(obj$area_boundaries$y))
  return(ls)
}

#' Adds limits based on the range of path plus and minus padding
#'
#' @param obj navr object
#' @param padding padding around the minmax of path
#'
#' @return list with xlim and ylim based on passed obj position limits and padding
#' @export
#'
#' @examples
geom_navr_path_limits <- function(obj, padding){
  padding <- c(-padding, padding)
  x <- range(obj$data$position_x) + padding
  y <- range(obj$data$position_y) + padding
  limits <- list(xlim(x), ylim(y))
  return(limits)
}

#' Plots positions of given events on a path
#' @details Searches the timestamps for the closes time (more then event time)
#' @description
#'
#' @param obj navr objects
#' @param event_times numeric vector of event times. Times need to be on the same scale as the timestamps in `obj$data$timestamp`
#' @param size point size **default** 2
#' @param shape point shape **default** is 4
#' @param color point color **default** is "blue"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_path_events <- function(obj, event_times, size = 2, shape = 18, color = "blue", ...){
  indices <- c()
  timestamps <- obj$data$timestamp
  for(t in event_times){
    i <- which(timestamps > t)[1]
    if(!is.na(i)) indices <- c(indices, i)
  }
  if(length(indices) == 0) return(list())
  df <- data.frame(x = obj$data$position_x[indices], y = obj$data$position_y[indices])
  return(geom_point(data=df, aes(x, y), size = size, shape=shape, color=color, ...))
}

#' Creates geom of a circle to be inserted into the graph
#'
#' @param center circle center as a numeric(2) *default*: c(0,0)
#' @param radius circle radius
#' @param precision how many points will make the circle
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_circle <- function(center, radius, precision = 100, ...){
  df_circle <- make_circle(center, radius, precision)
  return(geom_path(data = df_circle, aes(x, y), ...))
}

#' Adds timeseries to the given plot from navr object based on given column
#'
#' @details wrapper around `geom_navr_timeseries` for easier acces to the object
#'
#' @param obj NavrObject
#' @param colname name of the column to be plotted
#' @param scaling type of scaling to implemnent. "none", "std", "minmax"
#' @param scale if the values should be scaled to given values
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_obj_timeseries <- function(obj, colname, scaling = "none", scale = c(), ...){
  times <- get_times_since_start(obj)
  values <- obj$data[[colname]]
  return(geom_navr_timeseries(times, values, scaling, scale, ...))
}

#' Adds timeseries to the given plot
#'
#' @param times times on the x axis
#' @param values values on the y axis to plot
#' @param scaling type of scaling to implemnent. "none", "std", "minmax"
#' @param scale if the values should be scaled to given values
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_timeseries <- function(times, values, scaling = "none", scale = c(), ...){
  if(scaling == "std"){
    values <- scale(values)
  }
  if(scaling == "minmax"){

  }
  if(length(scale == 2)){
    #fits within a range
  }
  df <- data.frame(time = times, value = values)
  return(geom_line(data = df, aes(time, value), ...))
}

#' Draws vectical lines at times of given events
#'
#' @param event_times times of events, need to correspond to the X axis
#'
#' @return list if geom_vline
#' @export
#'
#' @examples
geom_navr_timeseries_events <- function(event_times, ...){
  ls <- list()
  for(time in event_times){
    ls <- c(ls, geom_vline(xintercept = time, ...))
  }
  return(ls)
}

#' geom to add stat_density2d position heatmap
#'
#' @param bins number of bins in each direction (n parameter in statn_density)
#' @param x position x
#' @param y position y
#' @param ... other ggpolot arguments
#'
#' @return
#' @export
#'
#' @examples
geom_position_heatmap <- function(x, y, bins = 25, ...){
  df <- data.frame(x=x, y=y)
  return(stat_density2d(data = df, aes(x, y, fill = stat(level)), n = bins, geom = "polygon", ...))
}

#' geom to add stat_density2d position heatmap
#'
#' @param obj navr object
#' @param bins number of bins in each direction (n parameter in statn_density)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_navr_heatmap <- function(obj, bins = 25, ...){
  return(geom_position_heatmap(obj$data$position_x, obj$data$position_y, bins, ...))
}

