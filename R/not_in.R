#' %nin%
#'
#' Returns a logical vector TRUE for elements of X not in Y
#'
#' Author: Holger Kirsten
#'
#' @param x Vector
#'
#' @param y Vector to compare against
#'
#' @export

"%nin%" <- function(x, y) {
    !(x %in% y)
}
