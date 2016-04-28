
#' Itemgetter
#'
#' Inspired by python's \code{operator.itemgetter}. Mostly useful for use with *apply functions.
#'
#' @param ... arbitrary indices passed to `[[` or `[`
#'
#' @return A function.
#' @export
#' @examples
#'
#' List <- list(1, 2:3)
#' Df <- data.frame(x=1, y=2:3)
#' Mat <- data.matrix(Df)
#'
#' ix <- Itemgetter(2)
#' ix(List)
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter(1:2)
#' ix(List)
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter(2, 2)
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter(2, 1:2)
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter(2, "y")
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter(2, TRUE)
#' ix(Df)
#' ix(Mat)
#'
#'
#' ix <- Itemgetter(2, 2, drop=FALSE)
#' ix(Df)
#' ix(Mat)
#'
#' ix <- Itemgetter("y")
#' ix(Df)
#'
#' Map(Itemgetter(2), list(List, Df, Mat))


Itemgetter <- function(...) {
  .args <- list(...)
  fn <- if(all(sapply(.args, function(x)
  (!is.logical(x) && length(x)==1)))) `[[` else `[`
  function(X) do.call(fn, c(list(X), .args))
}


#' Let
#'
#' Just a novelty function to define local variables for evaluation.
#'
#' @param ... see example
#'
#' @return Evaluated expression, which is the last argument passed to \code{Let}.
#' @export
#' @examples
#'
#' x <- 1
#' z <- 2
#'
#' ## instead of writing
#' with(list(x=2, y=3), z*(x+y))
#'
#' ## we can write
#' Let(x=2, y=3, z*(x+y))

Let <- function(...) {
  .args <- rev(as.list(match.call())[-1L])
  eval(.args[[1L]], .args[-1L], enclos = parent.frame())
}
