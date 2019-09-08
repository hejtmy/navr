#' Loads and checks position data into navr object
#'
#' @param obj
#' @param df dataframe with data to load. Navr expects particular columns to be present, check documentation on Navr object
#'
#' @return
#' @export
#'
#' @examples
load_position_data <- function(obj, df, ...){
  UseMethod("load_position_data")
}
#' @export
load_position_data.navr <- function(obj, df){
  if(is_navr_data(df)){
    obj$data <- df
  } else {
    warning("Couldn\'t load")
  }
  return(obj)
}

#' Validates and adds area boudaries
#'
#' @param obj navr object
#' @param ls list of list(x = c(0, 100), y=c(0,100)) type. Needs to have
#' x and y boundaries for the graphs to work properly
#'
#' @return modified object
#' @export
#'
#' @examples
add_area_boundaries <- function(obj, ls){
  obj$area_boundaries <- ls
  return(obj)
}

#' Checks if the data are valid
#'
#' @details checks if the dataframe has correct columns
#'
#' @param df dataframe to be checked
#'
#' @return bool
#' @export
#'
#' @examples
is_navr_data <- function(df){
  REQUIRED_COLUMNS <- c('timestamp', 'position_x', 'position_y')
  if(!all(REQUIRED_COLUMNS %in% colnames(df))){
    warning("Provided dataframe doesn\'t have all required columns.")
    return(FALSE)
  }
  #tests for data types
  return(TRUE)
}

#' Helper to rename column
#'
#' @param df input datafgrames
#' @param old_column old column name
#' @param new_column new column name
#'
#' @noRd
#' @return modified dataframe
rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df)==old_column] <- new_column
  return(df)
}
