% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_rew8r.R
\name{weights2corr}
\alias{weights2corr}
\title{Recalculate correlations and ranks based on new weights}
\usage{
weights2corr(
  COIN,
  w,
  aglevs = NULL,
  icodes = NULL,
  cortype = "pearson",
  withparent = TRUE
)
}
\arguments{
\item{COIN}{COIN object}

\item{w}{Full data frame of weights for each level}

\item{aglevs}{A 2-length vector with two aggregation levels to correlate against each other}

\item{icodes}{List of two character vectors of indicator codes, corresponding to the two aggregation levels}

\item{cortype}{Correlation type. Either \code{"pearson"} (default), \code{"kendall"} or \code{"spearman"}. See \link[stats:cor]{stats::cor}.}

\item{withparent}{Logical: if \code{TRUE}, only correlates with the parent, e.g. sub-pillars are only correlated with their parent pillars and not others.}
}
\value{
A list where \code{.$cr} is a vector of correlations between each indicator and the index, and
\code{.$dat} is a data frame of rankings, with unit code, and index, input and output scores
}
\description{
This is a short cut function which takes a new set of indicator weights, and recalculates the COIN results
based on these weights. It returns a summary of rankings and the correlations between indicators and index.
}
\details{
This function is principally used inside \code{\link[=rew8r]{rew8r()}}. The \code{w} argument should be a data frame of weights, of the same format
as the data frames found in \code{.$Parameters$Weights}.
}
\examples{
# build ASEM COIN up to aggregation
ASEM <- build_ASEM()
# get correlations between pillars (level 2) and index (level 4)
# original weights used just for demonstration, normally you would alter first.
l <- weights2corr(ASEM, ASEM$Parameters$Weights$Original, aglevs = c(2,4))

}
\seealso{
\itemize{
\item \code{\link[=rew8r]{rew8r()}} Interactive app for adjusting weights and seeing effects on correlations
\item \code{\link[=getCorr]{getCorr()}} Get correlations between indicators/levels
}
}
