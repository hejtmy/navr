#' Title
#'
#' @param xlim
#' @param ylim
#'
#' @return
#' @export
#'
#' @examples
create_plot <- function(xlim = NA, ylim = NA){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plt <- ggplot2::ggplot()
  if (!is.na(xlim)) plt <- plt + xlim(xlim)
  if (!is.na(ylim)) plt <- plt + ylim(ylim)
  return(plt)
}
