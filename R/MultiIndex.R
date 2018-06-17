
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
#'

MultiIndex <- function(x, ...) {
  UseMethod("MultiIndex")
}

#' @rdname MultiIndex
#' @export

MultiIndex.default <- function(x, collapse=", ", format=identity) {
  sprintf("(%s)", paste(format(x), collapse=collapse))
}

#' @rdname MultiIndex
#' @export

MultiIndex.matrix <- function(x, ...) {
  apply(x, 1, MultiIndex, ...)
}

#' @rdname MultiIndex
#' @export

## This converts factors to integers so use MultiIndex.matrix instead
## MultiIndex.data.frame <- function(x, ...) {
##   ## do.call(partial(mapply, compose(MultiIndex, c)), unname(x))
##   ## do.call(partial(mapply, compose(function(x) MultiIndex(x, ...), c)), unname(x))
##   ## fn <- purrr::compose(function(x) MultiIndex(x, ...), c)
##   dotargs <- list(...)
##   fn <- function(...) do.call(MultiIndex, c(list(c(...)), dotargs))
##   do.call(partial(mapply, fn), unname(x))
## }

MultiIndex.data.frame <- function(x, ...)
  MultiIndex(as.matrix(x), ...)

#' @rdname MultiIndex
#' @export

MultiIndex.list <- function(x, ...)
  MultiIndex(as.data.frame(x, stringsAsFactors=FALSE), ...)

#' ParseIndex2dataframe
#'
#' Create data frame from MultiIndex.
#'
#' @param x Vector of MultiIndex
#' @param sep (optional) separator
#' @param names (optional) names for data frame
#' @param convert (optional) \code{TRUE}/\code{FALSE}
#'
#' @return A character vector.
#' @export

ParseIndex2dataframe <- function(x, ...)
  UseMethod("ParseIndex2dataframe")

#' @rdname ParseIndex2dataframe
#' @export

ParseIndex2dataframe.default <- function(x, sep=", ", names=NULL, type.convert=TRUE) {
  if(is.factor(x))
    x <- as.character(x)
  ##
  df <- as.data.frame(
    do.call(rbind, strsplit(sub("^\\((.+)\\)$", "\\1", x), sep)),
    stringsAsFactors=FALSE,
    check.names=FALSE,
    row.names=if(anyDuplicated(x) > 0) NULL else x
  )
  ##
  if(is.character(names))
    base::names(df) <- names
  if(type.convert)
    df[] <- lapply(df, utils::type.convert, as.is=TRUE)
  df
}
