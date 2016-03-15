
#' First
#'
#' @param x a sequential collection
#'
#' @return first element
#' @export

First <- function(x)
  x[[1]]

#' Rest
#'
#' @param x a sequential collection
#'
#' @return collection with first element removed
#' @export

Rest <- function(x)
  x[-1]

#' Last
#'
#' @param x a sequential collection
#'
#' @return last element
#' @export

Last <- function(x)
  x[[length(x)]]
