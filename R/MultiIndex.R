
#' MultiIndex
#'
#' A kudgey way of constructing a multi-element key as a character vector.
#'
#' @param x vector
#' @param collapse (optional) separator
#' @param format (optional) additional method for formatting (provided with MultiIndex is called through apply(), for instance)
#'
#' @return A character vector.
#' @export

MultiIndex <- function(x, collapse=", ", format=identity) {
  sprintf("(%s)", paste(format(x), collapse=collapse))
}
