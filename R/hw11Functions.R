#' Calculate R-Squared
#'
#' @param pred numerical vector of predicted values
#' @param truth numerical vector of true values
#' @return R-squared value as a measure of model fitness. formula: R^2 = 1 - RSS/TSS
#' @examples 
#' yTrue = c(0,1,1,1)
#' 
#' r2(pred = c(0,1,1,1.1), truth = yTrue)
#' r2(pred = c(0,1,0.7,1), truth = yTrue)
r2 = function(pred, truth) {
  # calculate RSS
  # formula: RSS = sum[(y_i - yHat_i)^2]
  RSS = 0
  for (i in 1:length(truth)) {
    RSS = RSS + (truth[i] - pred[i])^2
  }

  # calculate TSS
  # formula: TSS = sum[(y_i - yBar)^2]
  TSS = 0
  meanY = mean(truth)
  for (i in 1:length(truth)) {
    TSS = TSS + (truth[i] - meanY)^2
  }

  # calculate R^2
  rSq = 1 - (RSS/TSS)
  return(rSq)
}

#' Calculate Adjusted R-Squared
#'
#' @param pred numerical vector of predicted values
#' @param truth numerical vector of true values
#' @param d number of predictor variables used in model
#' @return Adjusted r-squared value as a measure of model fitness. formula: Adjusted R^2 = 1 - ((RSS/(n-d-1))/(TSS/(n-1)))
#' @examples 
#' yTrue = c(0,1,1,1)
#' 
#' adjR2(pred = c(0,1,1,1.1), truth = yTrue, d = 1)
#' adjR2(pred = c(0,1,0.7,1), truth = yTrue, d = 2)
adjR2 = function(pred, truth, d) {
  # calculate RSS
  # formula: RSS = sum[(y_i - yHat_i)^2]
  RSS = 0
  for (i in 1:length(truth)) {
    RSS = RSS + (truth[i] - pred[i])^2
  }
  
  # calculate TSS
  # formula: TSS = sum[(y_i - yBar)^2]
  TSS = 0
  meanY = mean(truth)
  for (i in 1:length(truth)) {
    TSS = TSS + (truth[i] - meanY)^2
  }
  
  # calculate Adjusted R^2
  rSqAdj = 1 - (RSS/(length(truth) - d - 1)) / (TSS/(length(truth)-1))
  return(rSqAdj)
}