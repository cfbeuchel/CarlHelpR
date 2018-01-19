#' Show all duplicated values
#'
#' Show all duplications in a vector and not only one.
#'
#' Author: Holger Kirsten
#'
#' @param x Vector to in which duplicates will be searched
#'
#' @param index indicate whether to return numerical index of duplicated entries or logical vector
#'
#' @export

duplis <- function(x, index = T) {

  # check for length of given vector
  if (length(x) == 0) {
    return(0)
  }
  # create data.table with index
  dt <- data.table::data.table(my.vec = x, index = 1:length(x))

  # get the duplicates from R's base duplicated function
  duplicated.values <- dt[duplicated(my.vec), unique(my.vec)]

  if(index == T) {

    # find all entries matching the duplicated values
    duplicated.entries <- dt[my.vec %in% duplicated.values]

    # key by duplicated.entries
    data.table::setkey(duplicated.entries, my.vec)

    # return index
    return(duplicated.entries$index)

    # return logical vector the length of the input
  } else if(index == F) {

    # find all elements matching the duplicated values
    duplicates.logical <- is.element(dt$my.vec, duplicated.values)

    #return
    return(duplicates.logical)
  } else stop("Please set a valid index indicator: T/F")
}
