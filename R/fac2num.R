
#' Fac2Num
#'
#' Factor to numeric
#'
#' @param x factor vector
#' @param class \code{"numeric"} or \code{"integer"}
#'
#' @return A numeric vector.
#' @export
#'

Fac2Num <- function(x, class="numeric") {
  if(!is.factor(x))
    stop(">>> x is not a factor <<<")
  get(paste0("as.", class), "package:base")(levels(x))[x]
}
