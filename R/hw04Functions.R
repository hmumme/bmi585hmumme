#' Calculate Box-Muller Transformation
#'
#' @param n number of samples to take from the standard normal distribution
#' @return 2 vectors in matrix columns 1 and 2. x1 and x2 are independent, normally distributed random vectors with n values each
#' @export 
#' @examples
#' boxMuller(5)
#' boxMuller(100)
boxMuller <- function(n) {
  xMat = matrix(0,n,2) # create vector of n x 2 full of zeroes (columns for x1 and x2)
  u1 = runif(n)
  u2 = runif(n)
  for (i in 1:n) {
    x1 = sqrt(-2*log(u1[i]))*cos(2*pi*u2[i]) # calculate x1 with BoxMuller equation
    x2 = sqrt(-2*log(u1[i]))*sin(2*pi*u2[i]) # calculate x2 with BoxMuller equation
    xMat[i,1] = x1 # add x1 into output matrix
    xMat[i,2] = x2 # add x2 into output matrix
  }
  return(xMat)
}