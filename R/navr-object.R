#' Creates and returns navr object
#'
NavrObject <- function(){
  obj <- list()
  data <- NULL
  size <- NA
  class(obj) <- append(class(obj), "navr")
  return(obj)
}
