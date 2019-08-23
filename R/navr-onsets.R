#' Searches for times when the movement onset started
#'
#' @param obj
#' @param speed_threshold
#' @param min_duration
#' @param ...
#'
#' @return time since start of each event happening
#' @export
#'
#' @examples
search_onsets <- function(obj, speed_threshold, min_duration, ...){
  UseMethod("search_onsets")
}

#' Searches for movement onsets and returns time since start for each event
#'
#' @param obj navr object with calculated speeds
#' @param speed_threshold what is the speed considered to be the moving speed
#' @param min_duration in secouds how long should the person be moving
#' @param still_speed_threshold what is considered to be the still speed threshold. defualts to `speed_threshold``
#' @param still_duration how long before the onset should hte person be still in seconds. Default 0
#'
#' @return list with times (time since start) and durations of speed
#' @export
#'
#' @examples
search_onsets.navr <- function(obj, speed_threshold, min_duration = 0,
                               still_speed_threshold = speed_threshold,
                               still_duration = 0, return_duration = F){
  speeds <- get_speeds(obj)
  time_diffs <- get_time_diffs(obj)
  ls <- search_onsets_speeds_times(speeds, time_diffs, speed_threshold, min_duration, still_speed_threshold,
                                        still_duration, return_duration)
  time_since_start <- get_times_since_start(obj)
  return(list(time_since_start = time_since_start[ls$indices], durations=ls$durations))
}

search_onsets_speeds_times <- function(speeds, time_diffs, speed_threshold, min_duration,
                                 still_speed_threshold, still_duration, return_duration){
  df_moving <- calculate_is_moving_table(speeds, time_diffs, speed_threshold, still_speed_threshold)
  groups <- df_moving[df_moving$duration > min_duration & df_moving$is_moving == "yes", "group"]
  if(still_duration > 0){ #could be dropped, but we don't wanna run the for loop unless we need to
    still_groups <- integer(0)
    for(group in groups){
      # finds previous no group
      prev <- df_moving[df_moving$is_moving == "no" & df_moving$group %in% c(group-1, group-2), ]
      if(nrow(prev) < 1) next
      if(prev$duration > still_duration) still_groups <- c(still_groups, group)
    }
    groups <- still_groups
  }
  # if there are in_between groups, we consider as a start the in_between start
  if(still_speed_threshold != speed_threshold){
    # logical of length groups
    is_in_between <- df_moving$is_moving[groups-1] == "in_between"
    i_selected <- sort(c((groups-1)[is_in_between], groups[!is_in_between]))
    i_start <- df_moving$index[i_selected]
    durations <- df_moving$index[i_selected]
  } else {
    i_start <- df_moving$index[groups]
    durations <-df_moving$duration[groups]
  }
  return(list(indices = i_start, durations = durations))
}

calculate_is_moving_table <- function(speeds, time_diffs, speed_threshold, still_speed_threshold){
  df <- data.frame(speed = speeds, time_diff = time_diffs)
  df$is_moving <- "in_between"
  df$is_moving[df$speed >= speed_threshold] <- "yes"
  df$is_moving[df$speed < still_speed_threshold] <- "no"
  df$is_moving_group <- rep(1:length(rle(df$is_moving)$lengths), rle(df$is_moving)$lengths)
  df_index <- aggregate(1:nrow(df), by=list(group=df$is_moving_group), FUN=min)
  df_moving <- aggregate(df$time_diff, by=list(is_moving=df$is_moving, group=df$is_moving_group), FUN=sum)
  df_moving <- merge(df_moving, df_index, by="group")
  colnames(df_moving) <- c("group", "is_moving", "duration", "index")
  return(df_moving)
}
