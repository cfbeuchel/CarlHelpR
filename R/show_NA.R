#' Show Missing Values.
#'
#' Show all NAs per column in a data.table or data.frame
#'
#' Author: Holger Kirsten
#'
#' @param x Data.table or data.frame to look for missing values in
#'
#' @export

# show NAs within a data.frame
show_NA <- function(x) {

    # 15.6.15 als data.frame 7.7.15 apply statt sapply damit auch mit matrix funzend
    resi <- apply(x, 2, function(y) sum(is.na(y)))
    resi2 <- data.frame(var = names(resi),
                        NAs = as.vector(resi),
                        vals = nrow(x) - as.vector(resi))
    resi2
    # if(is.data.table(x)) { setDT(resi2) return(resi2)} else return(resi2)
}
