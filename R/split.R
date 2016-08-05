################################################################################
##
## split.R
## Authors: Satoshi Takahama (satoshi.takahama@epfl.ch)
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

#' Splitr, Splitc
#'
#' Split matrix by row or column
#'
#' @param x A matrix.
#'
#' @name Split

NULL

#' @rdname Split
#' @export

Splitr <- function(x, ...)
  UseMethod("Splitr")

#' @return \code{Splitr}: Split matrix by row; return list.
#'
#' @rdname Split
#' @export

Splitr.matrix <- function(x)
  setNames(split(x, row(x)), rownames(x))

#' @rdname Split
#' @export

Splitc <- function(x, ...)
  UseMethod("Splitc")

#' @return \code{Splitc}: Split matrix by column; return list..
#'
#' @rdname Split
#' @export

Splitc.matrix <- function(x)
  setNames(split(x, col(x)), colnames(x))
