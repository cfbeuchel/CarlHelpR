#' Color area under density function.
#'
#' Plot a density function (e.g. dnorm) and color a specified area under the curve
#'
#'
#' @param density_function The density function to daw. Choose from e.g. http://www.stat.umn.edu/geyer/old/5101/rlook.html
#'
#' @param color_area Area under the density curve to be colored.
#'
#' @param range_x X-Axis range of the plot
#'
#' @param fill_col Color of the specified area
#'
#' @param ... additional parameters for the density function
#'
#' @export

color_dens_area <- function(density_function, color_area = c(-3,3), range_x = c(-3,3), fill_col = "steelblue", ...) {

  # get coordinates on x, simple line
  cord.x <- c(color_area[1], seq(color_area[1], color_area[2], 0.01), color_area[2])

  # coordinates on y need to follow the specified density function
  cord.y <- c(0, eval(density_function)(seq(color_area[1], color_area[2], 0.01), ...), 0)

  # draw the curve
  graphics::curve(eval(density_function)(x, ...), xlim = range_x, xlab = "", ylab = "")

  # draw the colored polygon marking the area below the input value
  graphics::polygon(cord.x, cord.y, col = fill_col)
}
