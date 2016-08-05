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


#' PkgFilesCopy, PkgFilesSync
#'
#' Copy or sync package files (e.g., from \code{inst/}) to desired location.
#'
#' @param package [character] Name of package.
#' @param files [character] Name of package files.
#' @param path [character] Location to copy files. If present directory (\code{"."}) or directory exists, only contents are copied. Otherwise, a directory of this name is created and contents are copied there.
#' @param addargs [character] Arguments to rsync for \code{SyncPkgFiles}. Unused by \code{CopyPkgFiles}.
#'
#' @return Side effect of copying example files to \code{path}.
#' @export

PkgFilesCopy <- function(package, files, path=".") {
  CopyContents <- function(remote, local) {
    if(file.info(local)[["isdir"]]) {
      for(f in Sys.glob(file.path(remote,"*")))
        file.copy(f, local, recursive=TRUE)
    } else {
      file.copy(f, local)
    }
  }
  location <- system.file(files, package=package)
  if(file.info(location)[["isdir"]] && !file.exists(path))
      dir.create(path, recursive=TRUE)
  CopyContents(location, path)
}


#' @describeIn PkgFilesCopy Sync files.
#' @export

PkgFilesSync <- function(package, files, path=".", addargs="") {
  location <- system.file(files, package=package)
  if(file.info(location)[["isdir"]] && file.exists(path)) {
    system(sprintf("rsync -arvupP %s %s/ %s/", addargs, location, path))
  } else {
    system(sprintf("rsync -arvupP %s %s %s", addargs, location, path))
  }
}
