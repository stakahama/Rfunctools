#' ReadRDA
#'
#' Load RDA (.rda) file and return contents as a list.
#'
#' @param x RDA file
#'
#' @return List of (named) contents of RDA file.
#' @export

ReadRDA <- function(x) {
  load(x, e <- new.env())
  as.list(e)
}
