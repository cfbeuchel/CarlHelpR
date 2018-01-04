#' Find Intersect
#'
#' Find the common elemets
#'
#' Borrowed from this thread: https://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
#'
#' @param a First vector to find intersect with the remaining input-vectors of
#'
#' @param b Second vector to find intersect with the remaining input-vectors of
#'
#' @param ... additional vectors to find the common intersect of
#'
#' @export

intersect_all <- function(a, b, ...){
  Reduce(base::intersect, list(a, b, ...))
}

