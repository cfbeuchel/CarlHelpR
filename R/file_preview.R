#' This is the title.
#'
#' Show the first user specified number of rows and columns of a data frame or matrix.
#'
#' Author: Holger Kirsten
#'
#' @param d The data.table or data.frame that is to be previewed
#'
#' @param mydims The number of rows and columns to be displayed.
#'
#' @export

file_preview <- function(d, mydims = 5) {
    # 29.1.15 data.table included
    if ("data.table" %in% class(d)) {
        d[1:min(dim(d)[1], mydims), names(d)[1:min(dim(d)[2], mydims)], with = F]
    } else d[1:min(dim(d)[1], mydims), 1:min(dim(d)[2], mydims), drop = F]
}
