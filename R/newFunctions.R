#' Unscale
#'
#' @param x numerical object from scale() with or without attributes
#' @return un-scaled version of x. reversed centering/scaling of x, if any
#' @export
#' @examples
#' xS = scale(matrix(1:10, ncol = 2))
#' unscale(x = xS)
unscale = function(x) {
  u = x # initialize unscaled numerical object
  xScale = attr(x,"scaled:scale") # scaling attribute
  xCenter = attr(x,"scaled:center") # centering attribute
  
  # check for scale and centering attributes, adjust accordingly
  if (!is.null(xScale)) {
    u = x * xScale # multiply by original values for scaling
  }
  if (!is.null(xCenter)) {
    # iterate through each column of x to add center values
    for (i in 1:ncol(x)) {
      u[,i] = u[,i] + xCenter[i]
    }
  }
  return(u)
}

#' Approximate Data from N Principle Components
#'
#' @param x numerical dataframe, matrix, or tibble
#' @param npc number of Principle Components (PCs) to use in approximation
#' @return approximation of data x based on npc principal components
#' @export
#' @examples 
#' x = matrix(c(4,5,2,10,2,8,4,5,6), ncol = 3)
#' pcApprox(x, npc = 1)
pcApprox = function(x, npc) {
  xS = scale(x) # center and scale x
  xCov = cov(xS) # calculate covariance matrix
  eigX = eigen(xCov) # calculate eigenvectors and eigenvalues of covariance matrix
  
  ks = eigX$vectors # extract eigenvectors as pcs of X
  
  # make sure that npc is not 0 or greater than number of pcs for x
  if (npc > ncol(ks) | npc == 0) {
    print("npc is incompatible with x ... returning input")
    return(x)
  }
  
  k = ks[,1:npc] # extract npcs to get eigenvectors we will use
  
  scores = xS %*% k # calculate PC scores
  
  approxScaled = scores %*% t(k) # get approximation of data based on npcs
  
  # get original approximation using unscale() function
  attr(approxScaled,"scaled:center") = attr(xS, "scaled:center")
  attr(approxScaled,"scaled:scale") = attr(xS, "scaled:scale")
  approx = unscale(approxScaled)
  
  # add into original format with original colnames/rownames, return
  if (is.data.frame(x)) {
    df = data.frame(approx)
    colnames(df) = colnames(x)
    return(df)
  } else if (tibble::is_tibble(x)) {
    tib = tibble::tibble(approx)
    colames(tib) = colnames(x)
    return(tib)
  } else {
    return(matrix(c(approx), ncol = ncol(xS))) # return without attributes
  }
}

#' Create Lollipop Plots of Principal Component Loadings
#' 
#' @param x numerical dataframe, or tibble
#' @return plotting object for lollipop plots of principal components of x
#' @export
#' @examples 
#' x = data.frame(data = matrix(c(4,5,2,10,2,8,4,5,6), ncol = 3))
#' plot = pcLollipop(x)
pcLollipop = function(x) {
  # get principal components of x
  xS = scale(x) # center and scale x
  xCov = cov(xS) # calculate covariance matrix
  eigX = eigen(xCov) # calculate eigenvectors and eigenvalues of covariance matrix
  pcs = eigX$vectors # extract eigenvectors as pcs of X
  colnames(pcs) = paste0("PC",rep(1:ncol(pcs))) # create colnames
  
  # add into original format with column for variable names
  if (is.data.frame(x)) {
    df = data.frame(pcs)
    df["Variable"] = colnames(x)
  } else if (tibble::is_tibble(x)) {
    df = tibble::tibble(pcs)
  } else {
    print("please input x as a tibble or data.frame ... returning data")
    return(x)
  }
  
  # convert to long version for plotting
  df_long = df |> tidyr::pivot_longer(!Variable, names_to = "PC", values_to = "value")
  
  # generate plot object
  plot = ggplot::ggplot(df_long, aes(x = Variable, y = value)) + ggplot::geom_point(aes(color = PC)) + 
    ggplot::geom_segment(aes(x=Variable, xend=Variable, y=0, yend=value, color = PC)) + 
    ggplot::facet_grid(PC ~ ., scales = "free_y") + ggplot::theme(legend.position = "none")
  return(plot)
}

