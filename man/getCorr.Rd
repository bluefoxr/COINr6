% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_multivariate.R
\name{getCorr}
\alias{getCorr}
\title{Get different types of correlation matrices}
\usage{
getCorr(
  COIN,
  dset,
  icodes = NULL,
  aglevs = NULL,
  cortype = "pearson",
  pval = 0.05,
  withparent = TRUE,
  grouplev = NULL
)
}
\arguments{
\item{COIN}{The COIN object}

\item{dset}{The target data set}

\item{icodes}{An optional list of character vectors where the first entry specifies the indicator/aggregate
codes to correlate against the second entry (also a specification of indicator/aggregate codes).}

\item{aglevs}{The aggregation levels to take the two groups of indicators from. See \code{\link[=getIn]{getIn()}} for details. Defaults to indicator level.}

\item{cortype}{The type of correlation to calculate, either \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.}

\item{pval}{The significance level for including correlations. Correlations with \eqn{p > pval} will be returned as \code{NA}.
Default 0.05. Set to 0 to disable this.}

\item{withparent}{If \code{TRUE}, and \code{aglev[1] != aglev[2]}, will only return correlations of each row with its parent.}

\item{grouplev}{The aggregation level to group correlations by if \code{aglev[1] == aglev[2]}. By default, groups correlations into the
aggregation level above. Set to 0 to disable grouping and return the full matrix.}
}
\value{
A data frame of correlation values in long format. Correlations with \eqn{p > pval} will be returned as \code{NA}.
}
\description{
Helper function for getting correlations between indicators. This retrieves subsets of correlation
matrices between different aggregation levels, in different formats.
}
\details{
Note that this function can only call correlations within the same data set (i.e. only one data set in \code{.$Data}).
}
\examples{
# build ASEM COIN
ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
# correlations of indicators in Political pillar
corrs <- getCorr(ASEM, dset = "Raw", icodes = "Political", aglevs = 1)

}
\seealso{
\itemize{
\item \code{\link[=plotCorr]{plotCorr()}} Plot correlation matrices of indicator subsets
}
}
