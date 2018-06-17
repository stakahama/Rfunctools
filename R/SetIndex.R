################################################################################
##
## SetIndex.R
## Authors: Satoshi Takahama (satoshi.takahama@epfl.ch)
## Mar 2016
##
## -----------------------------------------------------------------------------
##
## This file is part of Rfunctools
##
## see LICENSE
##
################################################################################

##
## {SCRIPT_DESCRIPTION}
##

## -----------------------------------------------------------------------------

#' SetIndex, ResetIndex
#'
#' Set index like python's DataFrame.set_index(,inplace=False) but works with data frames and character matrices
#'
#' @param x [data.frame or character matrix] table
#' @param index [character] index number or name
#'
#' @return [data.frame or matrix]
#' @export

SetIndex <- function(x, ...)
  UseMethod("SetIndex")

#' @describeIn SetIndex Sets row name to value of \code{index} and removes \code{index} from matix/data frame.
#' @export

SetIndex.default <- function(x, index=colnames(x)[1]) {
  rownames(x) <- if(length(index) > 1) MultiIndex(x[index]) else x[[index]]
  jj <- setdiff(colnames(x), if(is.numeric(index)) colnames(x)[index]  else index)
  x[,jj,drop=FALSE]
}

#' @describeIn SetIndex Converts tibble object (which does not accept row names) to data frame before applying \code{SetIndex.default}.
#' @export

SetIndex.tbl_df<- function(x, ...) {
  SetIndex(as.data.frame(x), ...)
}


#' @rdname SetIndex
#' @export

ResetIndex <- function(x, ...)
  UseMethod("ResetIndex")

#' @describeIn SetIndex Reset index (like \code{dplyr::add_rownames} or pandas \code{DataFrame.reset_index(,inplace=False)}). Returns data frame with index in first position.
#' @export

ResetIndex.data.frame <- function(x, index="index") {
  cbind(setNames(list(row.names(x)), index), `row.names<-`(x, NULL))
}

#' @describeIn SetIndex Call as.data.frame on matrix and then apply ResetIndex.data.frame.
#' @export

ResetIndex.matrix <- function(x, ...) {
  ResetIndex(as.data.frame(x), ...)
}
