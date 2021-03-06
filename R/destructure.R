################################################################################
##
## destructure.R
## Author: Satoshi Takahama (satoshi.takahama@gmail.com)
##
## -----------------------------------------------------------------------------
##
## This file is part of Rfunctools
##
## Rfunctools is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Rfunctools is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Rfunctools.  If not, see <http://www.gnu.org/licenses/>.
##
################################################################################

#' DBind, `[<-.result`
#'
#' Assign elements of a list to multiple variable names in current namespace.
#'
#' This operation mimics the behavior of \code{destructuring-bind} (Lisp), sequence unpacking (Python), and \code{deal} (MATLAB). This R implementation is taken directly from G. Grothendieck's implementation of \code{List} as posted in R-help[1] but renamed as \code{DBind}. While returning multiple values from a function is considered unfunctional, the idea is to compose multiple operations (funciton invocation and subsequent assignment of list contents) into one.
#'
#' @param x function first argument
#' @param ... function additional argument
#' @param value values to be assigned in parent frame
#'
#' @return Produces side effect of assigning multiple values in the parent frame.
#'
#' @references
#'
#' [1] https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html
#'
#' @export
#'
#' @examples
#'
#' ## As provided by G. Grothendieck in R-help:
#' DBind[QR,,QRaux]  <- qr(c(1,1:3,3:1))
#' DBind[,Green,Blue]  <- col2rgb("aquamarine")
#'
#' ## Additional examples:
#' x <- list(a=1, b=2)
#' DBind[a, b] <- x

`[<-.result` <- function(x, ..., value) {
   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
 }
