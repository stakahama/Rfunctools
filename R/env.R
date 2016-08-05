
#' PopulateEnv
#'
#' Populates environment with functions in a file or directory.
#'
#' This function provides a means of keeping functions (and data) in separate namespaces. Creates and attaches named environment to search() path if it doesn't already exist.
#'
#' @param env.name environment name
#' @param path file(s) or directory containing R files
#' @param new T/F recreate environment
#' @param ... further arguments passed to \code{list.files}
#'
#' @return Side effect of populating environment with sourced file contents.
#' @export

PopulateEnv <- function (env.name, path, new=FALSE, ...) {

  ## manage environments
  env.exists <- env.name %in% search()
  if(env.exists && !new) {
    env <- as.environment(env.name)
  } else {
    env <- new.env()
  }
  if(env.exists && new)
    detach(env.name, character.only=TRUE)

  ## get file list if directory provided
  if(file.info(path[1])$isdir)
    path <- list.files(path[1], "\\.r$", full.names = TRUE, ignore.case = TRUE, ...)

  ## source files
  for(.path in path)
    sys.source(.path, env)

  ## place in position 2
  if(!env.exists)
    attach(env, name=env.name)

  ## return
  invisible()
}
