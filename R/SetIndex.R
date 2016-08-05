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

SetIndex.default <- function(x, index=names(x)[1]) {
  rownames(x) <- x[,index]
  jj <- (if(is.numeric(index)) -index else setdiff(colnames(x), index))
  x[,jj,drop=FALSE]
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
