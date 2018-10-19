#' Set .libPaths when working on an IMISE server
#'
#' This function simply sets the correct .libPaths for the IMISE compute servers. it also returns a basic path for working from inside or via remote
#'
#' @param onServer Logical. Are you working from within the IMISE network and have /net/ as your root? When F sets the basic path to /mnt/mount1/ for remote mounting
#'
#' @param serverName name of the folder in rpackages with the R-packages for the server you are working on
#'
#' @export
#'
imise_initialize <- function(onServer = T, serverName = NA){

  if(onServer == T & is.na(serverName)){
    stop("Please set a compute server name (i.e. the rpackages folder), when using an IMISE server.")
  }

  # define alternative package directory
  if (onServer == T) {
    .libPaths(paste0("/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/", serverName))
    bp <- "/net/ifs1/san_projekte/projekte/"
  } else {
    # basic path
    bp <- "/mnt/mount1/"
  }
  return(bp)
}
