

#' CartesianProd
#'
#' Cartesian product, based on \code{expand.grid} but with row-major expansion (rather than column-major convention of expand.grid).
#'
#' @param ... arbitrary arguments passed to expand.grid
#' @param stringsAsFactors default taken from getOption("stringsAsFactors")
#'
#' @return A data frame.
#' @export
#'

CartesianProd <- function(..., stringsAsFactors=getOption("stringsAsFactors")) 
  rev(do.call(expand.grid, c(rev(list(...)), list(stringsAsFactors=stringsAsFactors))))
