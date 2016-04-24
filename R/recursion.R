################################################################################
##
## recursion.R
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

#' Recursetree
#'
#' This function recurses a nested list to return every element of a node as a list element. One application is to obtain the parse tree of a function.
#'
#' @param x list object
#'
#' @return Parse tree of a function.
#'
#' @export
#'
#' @examples
#'
#' Foo <- function(x,y) {
#'   z <- x / (2 * y)
#' }
#'
#' parsetree <- Recursetree(body(Foo))

Recursetree <- function(x, fn=identity) {
  if(length(x)==1) x else
  lapply(fn(x), Recursetree, fn)
}
