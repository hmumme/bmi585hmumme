#' Calculate p-value from Two-Sided T-Test
#'
#' @param t t-test statistic
#' @param n sample size
#' @return p-value, or area under the t-density calculated from t statistic and sample size 
#' @examples
#' twoSidedT(3, 5)
#' twoSidedT(0.2, 2)
twoSidedT = function(t,n) {
  df = n -1 # calculate degrees of freedom
  p = 2*pt(t,df,lower.tail = FALSE)
  return(p)
}

#' Calculate p-value from Z-Score
#'
#' @param z z-score
#' @return p-value, or area under the z-density calculated from z score
#' @examples
#' twoSidedZ(3)
#' twoSidedZ(0.2)
twoSidedZ = function(z) {
p = 2*pnorm(z, lower.tail = FALSE)
return(p)
}

#' Calculate Effect Size
#'
#' @param x vector of values of length n
#' @param g vector of group designations (0 or 1) of length n, corresponding to vector x
#' @return effect size, or Cohen's d. formula: d = |mean0 - mean1| / sqrt((sd0^2 + sd1^2)/2)
#' @examples
#' effectSize(c(1.42, 8.10, 9.22), c(0,1,1))
#' effectSize(c(1.42, 8.10, 9.22, 20.92, 5.67, 0.45), c(0,0,1,1,0))
effectSize = function(x,g) {
  # calculate means for each group samples
  mean0 = mean(x[which(g == 0)])
  mean1 = mean(x[which(g == 1)])
  
  # calculate standard deviation for each group samples
  sd0 = sd(x[which(g == 0)])
  sd1 = sd(x[which(g == 1)])
  
  # check if sd0 or sd1 are NA, change to zero
  if (is.na(sd0)) sd0 = 0
  if (is.na(sd1)) sd1 = 0
  
  # calculate effect size using formula: |mean0 - mean1| / sqrt((sd0 ^ 2 + sd1 ^ 2)/2)
  eSize = abs(mean0 - mean1) / sqrt((sd0^2 + sd1^2)/2)
  
  return(eSize)
}

#' Perform Welch's T-Test
#'
#' @param x vector of values of length n
#' @param y vector of values of length n
#' @return returns vector of length 5 with Welch's T statistic, degrees of freedom, p-value, mean of group x, and mean of group y
#' @examples
#' welchT(c(1.42, 8.10, 9.22), c(10.4, 4.52, 3.44))
#' welchT(c(1.42, 8.10, 9.22, 20.92, 5.67, 0.45), c(1.99, 8.09, 9.13, 9.10, 5.55, 0.11))
welchT = function(x,y) {
  # get sample means
  meanX = mean(x)
  meanY = mean(y)
  
  # get sample sizes and degrees of freedom
  nX = length(x)
  nY = length(y)
  
  # get sample variances
  sX = var(x)
  sY = var(y)
  
  # get degrees of freedom
  df = (sX / nX + sY / nY)^2 / (((sX / nX)^2 / (nX - 1)) + (sY / nY)^2 / (nY - 1))
  
  # calculate t-statistic
  t = (meanX - meanY) / sqrt(sX/nX + sY/nY)

  # calculate p-value 
  pVal = pt(t, df, lower.tail = FALSE)

  
  # return test statistics
  stats = c(t, df, pVal, meanX, meanY)
  names(stats) = c("TestStatistic", "DF", "pValue", "meanX", "meanY")
  return(stats)
}