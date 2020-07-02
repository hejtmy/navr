#' Searches for movement onsets and returns time since start for each event
#'
#' @param obj Valid object
#' @param speed_threshold what is the speed considered to be the moving speed
#' @param min_duration in secouds how long should the person be moving
#' @param still_speed_threshold what is considered to be the still speed threshold.
#' *Defualts* to `speed_threshold``
#' @param still_duration how long before the onset should hte person be still in seconds.
#' *Defaults* to 0
#' @param pause_duration how long "non" moving can the person be to allow still counting as an onset?
#' In seconds. *Defaults* to 0
#' @param ...
#'
#' @return list with times, time since start, and durations of movement epochs
#' @export
#'
#' @examples
search_onsets <- function(obj, speed_threshold, min_duration, ...){
  UseMethod("search_onsets")
}
#' @describeIn search_onsets Searches onsets in navr object. THe navr object has to have
#' calculated speeds, times etc.
#' @export
search_onsets.navr <- function(obj, speed_threshold, min_duration = 0,
                               still_speed_threshold = speed_threshold,
                               still_duration = 0, pause_duration = 0){
  speeds <- get_speeds.navr(obj)
  # some form of validation in case speeds have not been calculated
  time_diffs <- get_time_diffs.navr(obj)
  res <- search_onsets_speeds_times(speeds = speeds, time_diffs = time_diffs,
                                   speed_threshold = speed_threshold,
                                   min_duration = min_duration,
                                   still_speed_threshold = still_speed_threshold,
                                   still_duration = still_duration,
                                   pause_duration = pause_duration)
  time_since_start <- get_times_since_start.navr(obj)
  return(list(time = obj$data$time[res$indices],
              time_since_start = time_since_start[res$indices],
              duration = res$durations))
}


#' Searches for movement stops and returns time since start for each event
#'
#' @param obj navr object with calculated speeds
#' @param speed_threshold what is the speed considered to be the moving speed
#' @param min_duration in secouds how long should the person be still
#' @param ...
#'
#' @return  list with times, time since start, and durations of stillness
#' @export
#'
#' @examples
search_stops <- function(obj, speed_threshold, min_duration, ...){
  UseMethod("search_stops")
}

#' @describeIn search_stops Searches stops in navr object. THe navr object has to have
#' calculated speeds, times etc.
#' @export
search_stops.navr <- function(obj, speed_threshold, min_duration = 0){
  speeds <- get_speeds.navr(obj)
  time_diffs <- get_time_diffs.navr(obj)
  res <- search_stops_speeds_times(speeds, time_diffs, speed_threshold, min_duration)
  time_since_start <- get_times_since_start.navr(obj)
  return(list(time = obj$data$time[res$indices],
              time_since_start = time_since_start[res$indices],
              duration = res$durations))
}

#' Searches for onsets based on speeds and time diffs.
#'
#' @param speeds vector of speeds
#' @param time_diffs vector of time diffs
#' @param speed_threshold what speedis considered moving?
#' @param min_duration  what is the minimum duration of movemnet
#' @param still_speed_threshold what is considered not moving (defaults to speed)
#' @param still_duration how long before movement should the person be still
#' @param pause_duration length time under the speed threshold which does not break the movement duration
#' @noRd
search_onsets_speeds_times <- function(speeds, time_diffs, speed_threshold, min_duration,
                                 still_speed_threshold, still_duration, pause_duration = 0){
  df_moving <- calculate_is_moving_table(speeds, time_diffs, speed_threshold, still_speed_threshold, pause_duration)
  groups <- df_moving[df_moving$duration > min_duration & df_moving$is_moving == "yes", "group"]
  if(still_duration > 0){ #could be dropped, but we don't wanna run the for loop unless we need to
    still_groups <- integer(0)
    for(group in groups){
      # finds previous no group
      prev <- df_moving[df_moving$is_moving == "no" & df_moving$group %in% c(group-1, group-2), ]
      # if the two previous groups were not no - it means they were yes and so there was no "stop"
      if(nrow(prev) < 1) next
      if(prev$duration > still_duration) still_groups <- c(still_groups, group)
    }
    groups <- still_groups
  }
  # if there are in_between groups, we consider as a start the in_between start
  # Some moving groups have "no" moving before them,  so those start when the is_moving is yes
  # But some have "in_between". We consider those to have started moving at the start of "in_between"
  if(still_speed_threshold != speed_threshold){
    # logical of length groups
    is_in_between <- df_moving$is_moving[groups-1] == "in_between"
    i_selected <- c((groups-1)[is_in_between], groups[!is_in_between])
    #duration is either sum of "in_between" and "yes" or just "yes" for those blocks which are preceded with "no"
    durations <- c(df_moving$duration[(groups-1)[is_in_between]] + df_moving$duration[(groups)[is_in_between]],
                   df_moving$duration[(groups)[!is_in_between]])
    i_start <- df_moving$index[i_selected]
  } else {
    i_start <- df_moving$index[groups]
    durations <- df_moving$duration[groups]
  }
  return(list(indices = i_start, durations = durations))
}

search_stops_speeds_times <- function(speeds, time_diffs, speed_threshold, min_duration){
  df_moving <- calculate_is_moving_table(speeds, time_diffs, speed_threshold, speed_threshold)
  groups <- df_moving[df_moving$duration > min_duration & df_moving$is_moving == "no", "group"]
  i_start <- df_moving$index[groups]
  durations <- df_moving$duration[groups]
  return(list(indices = i_start, durations = durations))
}

# Returns a table with "yes", "no" and "in_between" groups
# "Groups" are jsut indices of sections/groupings of consecutive "yes" or "no"
#' @param speeds
#' @param time_diffs
#' @param speed_threshold
#' @param still_speed_threshold
#' @param pause_duration . BEWARE, runs a for loop, so it might be slow.
#' SAME tolerance is applied for not moving and for moving
#' @noRd
calculate_is_moving_table <- function(speeds, time_diffs, speed_threshold,
                                      still_speed_threshold, pause_duration = 0){
  df <- data.frame(speed = speeds, time_diff = time_diffs)
  df$is_moving <- "in_between"
  df$is_moving[df$speed >= speed_threshold] <- "yes"
  df$is_moving[df$speed < still_speed_threshold] <- "no"
  df$is_moving_group <- rep(1:length(rle(df$is_moving)$lengths), rle(df$is_moving)$lengths)
  df_index <- aggregate(1:nrow(df), by=list(group=df$is_moving_group), FUN=min)
  df_moving <- aggregate(df$time_diff, by=list(is_moving=df$is_moving, group=df$is_moving_group), FUN=sum)
  if(pause_duration > 0){
    df_moving <- fill_in_pauses(df_moving, pause_duration, "yes")
    df_moving <- fill_in_pauses(df_moving, pause_duration, "no")
  }
  df_moving <- merge(df_moving, df_index, by="group")
  colnames(df_moving) <- c("group", "is_moving", "duration", "index")
  # if we merged, we need to reindex due to how the search works
  df_moving <- df_moving[order(df_moving$index),]
  df_moving$group <- 1:nrow(df_moving)
  return(df_moving)
}

fill_in_pauses <- function(df_moving, pause_duration, group){
  i_group <- which(df_moving$is_moving == group)
  for(i in 2:length(i_group)){
    i_current <- i_group[i]
    i_previous <- i_group[i-1]
    i_between <- (i_previous+1):(i_current-1)
    break_duration <- sum(df_moving$x[i_between])
    if(break_duration < pause_duration){
      # adds the break length to the duration
      df_moving$x[i_previous] <- df_moving$x[i_previous] + df_moving$x[i_current] + break_duration
      group <- df_moving$group[i_previous]
      #changes group index to the first group
      df_moving$group[c(i_between, i_current)] <- group
      # This weird indexing is here to allow also changinbg all previous
      # filled in breaks.
      df_moving[df_moving$group == group, ] <- df_moving[i_previous,]
    }
  }
  df_moving <- unique(df_moving)
  return(df_moving)
}
