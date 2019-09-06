#' Navr Object
#' @details Creates empty navr object with prefilled fields
#'
#' @description NavrObject is a list of class `navr` which contains two main fields
#'
#' - **area_aboundaries**: list with (x, y, z) fields, each containinng numeric(2) definition of limits (or NULL if empty).
#' Used mainly for plotting and removal of outlier points
#'
#' - **data**: data.frame with few mandatory and some optional fields
#'
#'   *Mandatory columns*:
#'   - timestamp
#'   - position_x
#'   - position_y
#'
#'   *Optional columns*: Columns which can be automatically preprocessed if need be
#'   - position_z: For 3D positional data
#'   - position_(name)_(axis): Position columns can be automatically preprocessed and have distances etc. calculated if they
#'   follow given naming structure e.g. positions_vrhead_x
#'   - rotation_(name)_(axis): Rotation columns can be automatically preprocessed if then follow given naming structure
#'
#'   *Computed columns*: These columns are computed automatically and ovewrite any existing columns of given names. Please don't namme
#'   your custom columns with these names
#'   - time_since_start
#'   - time_diff
#'   - distance
#'   - distance_total
#'   - speed
#'   - rotation_(name)_(axis)_diff
#'
#'   *Additional columns*: the data fame can have unlimited number of additional columns, if they don't interfere
#'   with naming of the mandatory and optional (e.g. you shouldn't have a column named rotation_controller if you don't want
#'   it preprocessed by the package). Typical examples of additional columns can be "fps", "input" etc.
#'
#' @return navr object
#' @export
#'
#' @examples
NavrObject <- function(){
  obj <- list()
  obj$data <- data.frame()
  obj$area_boundaries <- list(x = NULL, y = NULL, z = NULL)
  class(obj) <- append(class(obj), "navr")
  return(obj)
}
