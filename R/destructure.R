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

#' Destructure, `[<-.result`
#'
#' Mimick behavior of destructuring bind (so named in Lisp). Also called  sequence unpacking in Python, and implemented by \code{deal} in MATLAB.
#' This definition is taken directly from G. Grothendieck's implementation of \code{List} as posted in R-help[1].
#'
#' @param x function first argument
#' @param ... function additional argument
#' @param value values to be assigned in parent frame
#'
#' @return produces side effect of assignment of multiple values in the parent frame
#'
#' @references
#'
#' https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html
#'
#' @export
#'
#' @examples
#'
#' ## As provided by G. Grothendieck in R-help:
#' Destructure[QR,,QRaux]  <- qr(c(1,1:3,3:1))
#' Destructure[,Green,Blue]  <- col2rgb("aquamarine")

Destructure <- structure(NA,class="result")

"[<-.result" <- function(x,...,value) {
   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
}

