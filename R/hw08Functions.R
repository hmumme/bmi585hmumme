#' Perform Bonferroni-Holm Adjustment, Report significant p-values
#'
#' @param p vector of unadjusted p-values
#' @return logical vector showing which values from p are statistically significant after Bonferroni-Holm Adjustment (assuming alpha = 0.05)
#' @examples
#' bhAdjust(c(0.0025, 0.0050, 0.0075, 0.0100, 0.0125))
#' bhAdjust(c(0.05, 0.0005, 0.01, 0.0225, 0.025))
bhAdjust = function(p) {
  # assuming alpha = 0.05
  pOut = p.adjust(p, method = "holm")
  return(pOut < 0.05)
}

#' Perform FDR Adjustment, Report Significant p-values
#'
#' @param p vector of unadjusted p-values
#' @return logical vector showing which values from p are statistically significant after FDR Adjustment (assuming alpha = 0.05)
#' @examples
#' fdrAdjust(c(0.0025, 0.0050, 0.0075, 0.0100, 0.0125))
#' fdrAdjust(c(0.05, 0.0005, 0.01, 0.0225, 0.025))
fdrAdjust = function(p) {
  # assuming alpha = 0.05
  pOut = p.adjust(p, method = "fdr")
  return(pOut < 0.05)
}