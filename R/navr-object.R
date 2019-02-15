#' Navr Object
#'
#' @return navr object
#' @export
#'
#' @examples
NavrObject <- function(){
  obj <- list()
  data <- NULL
  size <- NA
  class(obj) <- append(class(obj), "navr")
  return(obj)
}
