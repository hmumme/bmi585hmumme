#' Calculate minimum number of samples
#'
#' @param d target effect size 
#' @return minimum number of samples needed to achieve effect size d and power = 0.8
#' @export
#' @examples
#' minimumN(1.3)
#' minimumN(0.5)
minimumN = function(d) {
  n = power.t.test(power = 0.8, delta = d)$n # find minimum sample size
  return(ceiling(n))
}

#' Perform ChiSquare Test of Homogeneity
#'
#' @param tib tibble dataframe with colnames that contain x and y
#' @param x column x of tib, variable 1 of comparison 
#' @param y column y of tib, variable 2 of comparison
#' @return p-value from ChiSquare Test of Homogenity between data in columns x and y of tib
#' @export
#' @examples
#' data = tibble::tibble("sex" = c(1,0,1),"group" = c("A","B","C"), "age" = c(40,15,33), "height" = c(63, 70, 68))
#' 
#' chiSquareCounts(data, "sex", "group")
#' chiSquareCounts(data, "age", "height")
chiSquareCounts = function(tib, x, y) {
  # create count table with x variables as columns and y variables as rows
  tab = table(data[[y]], data[[x]])
  
  # perform chi-squared test of homogenity:
  
  # get sum values for calculations
  sumRows = rowSums(tab) # create vector of row sums
  sumCols = colSums(tab) # create vector of column sums
  sumTotal = sum(tab) # total sum for entire table
  
  # create expected counts table
  expCounts = tab
  for (r in 1:nrow(expCounts)) {
    for (c in 1:ncol(expCounts)) {
      expCounts[r,c] = sumRows[r] * sumCols[c] / sumTotal
      
    }
  }
  
  # calculate chi square statistic  = sum [(observed - expected)^2 / expected ]
  chiStat = sum((tab - expCounts)^2 / expCounts)
  
  # calculate p-value from chi-square statistic
  df = (nrow(tab) - 1) * (ncol(tab) - 1)
  p = pchisq(chiStat, df = df, lower.tail = FALSE)
  return(p)
}

#' Estimate of Post Hoc Power from 1000 Simulations
#'
#' @param d effect size (Cohen's d)
#' @param n1 number of samples in group 1
#' @param n2 number of samples in group 2
#' @return estimate of post hoc power from 1000 simulations. formula: power estimate = # statistically significant simulations / total # of simulations
#' @export
#' @examples
#' postHocPower(d = 1, n1 = 100, n2 = 90)
#' postHocPower(d = 0.5, n1 = 20, n2 = 22)
postHocPower = function(d, n1, n2) {
  # assuming alpha = 0.05
  # we can generate two groups with n1,n2 values, that have means and sd according to our d
  # assuming group1 is normally distributed with mean = 0 and sd = 1
  mean1 = 0
  sd1 = 1
  
  # solve for group2 parameters
  sd2 = sqrt((1 + 1)/2) # assuming equal variances, we can calculate for pooled sd
  mean2 = -1 * sd2 * (d - mean1/sd1) # we know mean1, sd1, sd2, and d so we calculate for mean2
  
  # repeat t-test between random variables group1 and group2 1000 times, store p-values
  p = rep(0,1000)
  for (i in 1:1000) {
    # generate groups using calculated values for n_per_group and d
    group1 = rnorm(n1, mean = mean1, sd = sd1)
    group2 = rnorm(n2, mean = mean2, sd = sd2)
    
    # perform a t.test to determine if there is a difference between group1 and group2, store p-value
    p[i] = t.test(group1, group2)$p.value
  }
  
  # calculate power
  numSig = length(which(p < 0.05))
  numTotal = length(p)
  
  return(numSig / numTotal)
}

