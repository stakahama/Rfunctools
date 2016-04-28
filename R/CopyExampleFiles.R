################################################################################
##
## CopyExampleFiles.R
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


#' CopyPkgFiles, SyncPkgFiles
#'
#' Copy or sync package files (e.g., from \code{inst/}) to desired location.
#'
#' @param path [character] Location to copy files. If present directory (\code{"."}) or directory exists, only contents are copied. Otherwise, a directory of this name is created and contents are copied there.
#' @param addargs [character] Arguments to rsync for \code{SyncPkgFiles}. Unused by \code{CopyPkgFiles}.
#' @param files [character] Name of package files.
#' @param package [character] Name of package.
#'
#' @return Side effect of copying example files to \code{path}.
#' @export

CopyPkgFiles <- function(path=".", addargs, files, package) {
  CopyContents <- function(remote, local) {
    if(file.info(local)[["isdir"]]) {
      for(f in Sys.glob(file.path(remote,"*")))
        file.copy(f, local, recursive=TRUE)
    } else {
      file.copy(f, local)
    }
  }
  location <- system.file(files, package=package)
  if(file.info(location)[["isdir"]])
    if(!file.exists(path)) {
      parts <- split(path, .Platform$file.sep)[[1]]
      for(i in seq_along(parts)) {
        current.path <- do.call(file.path, as.list(parts[1:i]))
        if(!file.exists(current.path))
          dir.create(current.path)
      }
    }
  CopyContents(location, path)
}


#' @describeIn CopyPkgFiles Sync files
#' @export

SyncPkgFiles <- function(path=".", addargs="", files, package) {
  location <- system.file(files, package=package)
  if(file.info(location)[["isdir"]] && file.exists(path)) {
    system(sprintf("rsync -arvupP %s %s/ %s/", addargs, location, path))
  } else {
    system(sprintf("rsync -arvupP %s %s %s", addargs, location, path))
  }
}
