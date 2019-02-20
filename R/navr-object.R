#' Navr Object
#'
#' @return navr object
#' @export
#'
#' @examples
NavrObject <- function(){
  obj <- list()
  obj$data <- NULL
  obj$area_boundaries <- NULL
  class(obj) <- append(class(obj), "navr")
  return(obj)
}
