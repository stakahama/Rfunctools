

#' NamedList
#'
#' Automatically label list elements if names are not provided
#'
#' @param ... list elements
#'
#' @return List with named elements.
#' @export
#' @examples
#'
#' a <- 3
#' out <- NamedList(a, b=1)

NamedList <- function(...){
  .call <- as.character(match.call()[-1L])
  .args <- list(...)
  if(is.null(names(.args))) {
    names(.args) <- .call
  } else {
    ix <- !nchar(names(.args))
    names(.args)[ix] <- .call[ix]
  }
  .args
}
