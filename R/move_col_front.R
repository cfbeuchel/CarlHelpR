#' Move A Column To The Front
#'
#' Move a single column of a data frame to the very front of it, i.e. set it as the first column.
#'
#' Author: Holger Kirsten
#'
#' @param d Data.table or data.frame whose columns are to be rearranged.
#'
#' @param colname Character string giving the column name to be rearranged as first column
#'
#' @export

move_col_front <- function(d, colname = "colname") {
    ## 15.6.15 data.table auf setcolorder umgestellt multiple ohne warning
    stopifnot(all(colname %in% names(d)))
    index <- match(colname, names(d))
    old_order <- 1:ncol(d)
    new_order <- c(index, old_order[old_order %nin% index])
    if (data.table::is.data.table(d))
      data.table::setcolorder(d, new_order)
    if (data.table::is.data.table(d) == F)
        d = d[, new_order]
    return(d)
}
