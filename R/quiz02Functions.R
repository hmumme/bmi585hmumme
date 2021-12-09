#' Calculate sensitivity
#'
#' @param pred number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)
#' @param truth number or boolean vector of true values (0 or F = negative, 1 or T = positive)
#' @return The sensitivity, formula: sensitivity = # True Positives / (# True Positives + # False Negatives)
#' @examples
#' sensitivity(c(0,1,0),c(1,1,1))
#' sensitivity(c(1,1,1),c(1,1,0))
sensitivity = function(pred, truth) {
  tp = 0 # initialize true positive value
  fn = 0 # initialize false negative value
  
  # if pred and truth are logical vectors, convert to numeric 0 for negative/False, 1 for positive/True
  if (is.logical(pred) | is.logical(truth)) {
    pred = as.numeric(pred) # convert pred to 0s and 1s
    truth = as.numeric(truth) # convert truth to 0s and 1s
  }
  
  # iterate through each pred and truth value and compare
  for (i in 1:length(pred)) {
    # check if truth[i] is positive, then compare with pred
    if (truth[i] == 1 & pred[i] == 1) {
      tp = tp + 1 # add to true positive counter
    } else if (truth[i] == 1 & pred[i] == 0) {
      fn = fn + 1 # add to false negative counter
    }
  }
  s = tp / (tp + fn) # calculate sensitivity
  if (is.nan(s)) print("warning: no positives found in truth vector.") # print if no positives in truth vector, cannot calculate sp
  return(s)
}

#' Calculate specificity
#'
#' @param pred number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)
#' @param truth number or boolean vector of true values (0 or F = negative, 1 or T = positive)
#' @return The specificity, formula: specificity = # True Negatives / (# True Negatives + # False Positives)
#' @examples
#' specificity(c(0,1,0),c(1,0,1))
#' specificity(c(1,1,1),c(1,0,0))
specificity = function(pred, truth) {
  tn = 0 # initialize true negative value
  fp = 0 # initialize false positive value
  
  # if pred and truth are logical vectors, convert to numeric 0 for negative/False, 1 for positive/True
  if (is.logical(pred) | is.logical(truth)) {
    pred = as.numeric(pred) # convert pred to 0s and 1s
    truth = as.numeric(truth) # convert truth to 0s and 1s
  }
  
  # iterate through each pred and truth value and compare
  for (i in 1:length(pred)) {
    # check if truth[i] is negative, then compare with pred
    if (truth[i] == 0 & pred[i] == 0) {
      tn = tn + 1 # add to true negative counter
    } else if (truth[i] == 0 & pred[i] == 1) {
      fp = fp + 1 # add to false positive counter
    }
  }
  s = tn / (tn + fp) # calculate specificity
  return(s)
}

#' Calculate accuracy
#'
#' @param pred number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)
#' @param truth number or boolean vector of true values (0 or F = negative, 1 or T = positive)
#' @return The accuracy, formula: accuracy = (# True Positives + # True Negatives) / (# All Positives + # All Negatives)
#' @examples
#' accuracy(c(0,1,0),c(1,0,1))
#' accuracy(c(1,1,1),c(1,0,0))
accuracy = function(pred, truth) {
  tp = 0 # initialize true positive value
  tn = 0 # initialize true negative value
  
  # if pred or truth are logical vectors, convert to numeric 0 for negative/False, 1 for positive/True
  if (is.logical(pred) | is.logical(truth)) {
    pred = as.numeric(pred) # convert pred to 0s and 1s
    truth = as.numeric(truth) # convert truth to 0s and 1s
  }
  
  ap = length(which(pred == 1)) # calculate number of positives in predicted
  an = length(which(pred == 0)) # calculate number of negatives in predicted
  
  # iterate through each pred and truth value and compare
  for (i in 1:length(pred)) {
    # check if truth[i] is negative, then compare with pred
    if (truth[i] == 0 & pred[i] == 0) {
      tn = tn + 1 # add to true negative counter
    } else if (truth[i] == 1 & pred[i] == 1) {
      tp = tp + 1 # add to true positive counter
    }
  }
  a = (tn + tp) / (an + ap) # calculate accuracy
  return(a)
}

#' Calculate PPV
#'
#' @param pred number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)
#' @param truth number or boolean vector of true values (0 or F = negative, 1 or T = positive)
#' @return The PPV, formula: PPV = # True Positives / (# True Positives + # False Positives)
#' @examples
#' ppv(c(0,1,0),c(1,0,1))
#' ppv(c(1,1,1),c(1,0,0))
ppv = function(pred, truth) {
  tp = 0 # initialize true positive value
  fp = 0 # initialize false positive value
  
  # if pred or truth are logical vectors, convert to numeric 0 for negative/False, 1 for positive/True
  if (is.logical(pred) | is.logical(truth)) {
    pred = as.numeric(pred) # convert pred to 0s and 1s
    truth = as.numeric(truth) # convert truth to 0s and 1s
  }
  
  # iterate through each pred and truth value and compare
  for (i in 1:length(pred)) {
    # check if truth[i] is positive, then compare with pred
    if (truth[i] == 1 & pred[i] == 1) {
      tp = tp + 1 # add to true positive counter
    } else if (truth[i] == 0 & pred[i] == 1) {
      fp = fp + 1 # add to false positive counter
    }
  }
  p = tp / (tp + fp) # calculate PPV
  return(p)
}

#' Calculate F1 Score
#'
#' @param pred number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)
#' @param truth number or boolean vector of true values (0 or F = negative, 1 or T = positive)
#' @return The F1 Score, formula: F1 = 2 * (precision * recall) / (precision + recall)
#' @examples
#' f1(c(0,1,0),c(1,0,1))
#' f1(c(1,1,1),c(1,0,0))
f1 = function(pred, truth) {
  precision = ppv(pred, truth) # calculate PPV (precision)
  recall = sensitivity(pred, truth) # calculate Sensitivity (recall)
  
  f = 2 * (precision*recall) / (precision+recall) # calculate F1
  return(f)
}
