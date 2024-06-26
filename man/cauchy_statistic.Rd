% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cauchy_statistic.R
\name{cauchy_statistic}
\alias{cauchy_statistic}
\title{Calculate Cauchy Combination Test Statistic}
\usage{
cauchy_statistic(p_values, weights = NULL)
}
\arguments{
\item{p_values}{A numeric vector of p-values.}

\item{weights}{A numeric vector of weights corresponding to the p-values. If NULL, equal weights are used.}
}
\value{
A numeric value representing the CCT statistic.
}
\description{
This function calculates the Cauchy Combination Test (CCT) statistic for a given set of p-values and optional weights.
}
\details{
The CCT statistic is calculated as a weighted sum of the tangents of the p-values (transformed).
If any p-value is zero, the statistic is zero. If any p-value is very close to one, the statistic is calculated
using the minimum p-value approach.
}
\examples{
p_values <- c(0.01, 0.02, 0.03, 0.04)
weights <- c(0.4, 0.3, 0.2, 0.1)
cct_stat <- cauchy_statistic(p_values, weights)
print(cct_stat)
}
