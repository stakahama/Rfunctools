################################################################################
##
## extra.R
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

#' PreserveAttr
#'
#' This function returns a function that retains the desired attribute of its first argument.
#' 
#' @param .which name of attribute to preserve from first argument of returned function
#'
#' @return A new function which accepts the function to be applied as its first argument, and its arguments follow. The first of these arguments will be used to obtain the attribute to be copied to the returned object.
#'
#' @export
#'
#' @examples
#'
#' label <- sprintf("V%d",seq(3))
#'
#' id.float <- setNames(seq(.1,.3,.1),label)
#'
#' var.1 <- setNames(1:3,label)
#' attr(var.1,"id") <- id.float
#'
#' var.2 <- setNames(4:6,label)
#' attr(var.2,"id") <- id.float
#'
#' PreserveAttr("id")(rbind,var.1,var.2)

PreserveAttr <- function(.which) function(fn,...) {
  arg.first <- list(...)[[1]]
  out <- fn(...)
  `attr<-`(out,.which,attr(arg.first,.which))
}


#' AssignAttr
#'
#' Applies arbitrary setter functions to an object within a local scope and returns the object with attributes set.
#' 
#' @param x object
#' @param attr name of attribute (character)
#' @param value value of attribute
#'
#' @return The same object with attribute set to desired value.
#'
#' @export
#'
#' @examples
#'
#' a <- cbind(1:2)
#' colnames(a) <- "numbers"
#' print(a)
#'
#' b <- AssignAttr(cbind(1:2), "colnames", "numbers")
#'

AssignAttr <- function(x, attr, value) {
  get(paste0(attr, "<-"))(force(x), value)
}
