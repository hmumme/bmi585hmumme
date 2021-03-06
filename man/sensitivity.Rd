% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quiz02Functions.R
\name{sensitivity}
\alias{sensitivity}
\title{Calculate sensitivity}
\usage{
sensitivity(pred, truth)
}
\arguments{
\item{pred}{number or boolean vector of predicted values (0 or F = negative, 1 or T = positive)}

\item{truth}{number or boolean vector of true values (0 or F = negative, 1 or T = positive)}
}
\value{
The sensitivity, formula: sensitivity = # True Positives / (# True Positives + # False Negatives)
}
\description{
Calculate sensitivity
}
\examples{
sensitivity(c(0,1,0),c(1,1,1))
sensitivity(c(1,1,1),c(1,1,0))
}
