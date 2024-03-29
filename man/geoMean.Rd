% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_aggregate.R
\name{geoMean}
\alias{geoMean}
\title{Weighted geometric mean}
\usage{
geoMean(x, w = NULL)
}
\arguments{
\item{x}{A numeric vector of positive values.}

\item{w}{A vector of weights, which should have length equal to \code{length(x)}. Weights are relative
and will be re-scaled to sum to 1. If \code{w} is not specified, defaults to equal weights.}
}
\value{
The geometric mean, as a numeric value.
}
\description{
Weighted geometric mean of a vector. \code{NA} are skipped by default. This function is used inside
\code{\link[=aggregate]{aggregate()}}.
}
\examples{
# a vector of values
x <- 1:10
# a vector of weights
w <- runif(10)
# weighted geometric mean
geoMean(x,w)

}
