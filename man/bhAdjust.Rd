% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hw08Functions.R
\name{bhAdjust}
\alias{bhAdjust}
\title{Perform Bonferroni-Holm Adjustment, Report significant p-values}
\usage{
bhAdjust(p)
}
\arguments{
\item{p}{vector of unadjusted p-values}
}
\value{
logical vector showing which values from p are statistically significant after Bonferroni-Holm Adjustment (assuming alpha = 0.05)
}
\description{
Perform Bonferroni-Holm Adjustment, Report significant p-values
}
\examples{
bhAdjust(c(0.0025, 0.0050, 0.0075, 0.0100, 0.0125))
bhAdjust(c(0.05, 0.0005, 0.01, 0.0225, 0.025))
}
