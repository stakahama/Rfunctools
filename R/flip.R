################################################################################
##
## flip.R
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

#' Flip
#'
#' Rearranges order of named function arguments (excludes dotted argument list). By default, reverses order of named function arguments . If \code{pos} argument is specified, these arguments are arranged to come first. Useful with *apply() functions.
#'
#' @param fn a function
#' @param pos position number or argument name
#'
#' @return A function in which the position of arguments is ordered.
#'
#' @export
#'
#' @examples
#'
#' Foo <- function(x,y,z)
#'   x / y + z
#'
#' Flip(Foo)
#' Flip(Foo,"z")
#' Flip(Foo,2:3)
#' Flip(Foo,c("y","z"))


Flip <- function(fn, pos) {

  args <- formals(fn)
  if(missing(pos)) {
    pos <- setdiff(rev(seq(length(args))), grep("...", names(args), fixed=TRUE))
  } else if(is.character(pos)) {
    pos <- sapply(sprintf("^%s$", pos), grep, names(args), USE.NAMES=FALSE)
  }

  ## error checking
  if(length(pos)==0)
    stop("'pos' is zero")
  if(any(pos > length(args)))
    stop("'pos' is greater than number of arguments")

  fn.copy <- fn # copy
  formals(fn.copy) <- c(args[pos], args[-pos])
  fn.copy
}
