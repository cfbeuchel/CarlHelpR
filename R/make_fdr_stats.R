#' This is the title.
#'
#' A wrapper to compute FDR for a list of pvalues.
#'
#' Author: Holger Kirsten
#'
#' @param pval A list or vector of Pvalues to be passed to the fdrtool function
#'
#' @param ... ...
#'
#' @export

# summarize the results from the fdrtool (Korbinian)
make_fdr_stats <- function(pval, ...) {
  stopifnot(is.vector(pval))
  res1 <- fdrtool::fdrtool(pval, statistic = "pvalue", ...)
  res <- data.frame(res1$param)
  res$eta1 <- 1-res$eta0
  res$min_p <- min(pval)
  res$p_kl_bonf <- sum(pval <= 0.05/length(pval))
  res$p_kl_001 <- sum(pval <= 0.001)
  res$p_kl_01 <- sum(pval <= 0.01)
  res$min_fdr <- min(res1$qval)
  res$q_kl_05 <- sum(res1$qval <=0.05)
  res$q_kl_20 <- sum(res1$qval <=0.20)
  res$n <- length(pval)
  return(res)
}
