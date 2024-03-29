% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_assemble.R
\name{assemble}
\alias{assemble}
\title{Build COIN object}
\usage{
assemble(
  IndData,
  IndMeta,
  AggMeta,
  include = NULL,
  exclude = NULL,
  preagg = NULL,
  use_year = NULL,
  impute_latest = FALSE
)
}
\arguments{
\item{IndData}{A data frame of indicator data.}

\item{IndMeta}{A data frame containing auxiliary information for each indicator}

\item{AggMeta}{A data frame specifying the names and weights of each aggregation group}

\item{include}{Optional argument specifying a subset of indicator codes to include (default all indicators included)}

\item{exclude}{Optional argument specifying a subset of indicator codes to exclude (default none excluded)}

\item{preagg}{Set to \code{TRUE} if you want to assemble a COIN using pre-aggregated data (typically for ex-post analysis)}

\item{use_year}{If \code{IndData} includes a \code{Year} column, and there are multiple observations for each unit (one per year),
this can be set to a target year or years. For example, setting \code{use_year = 2020} will filter \code{IndData} to only include points from
2020. Setting to \code{use_year = c(2019,2020)} will return a list of COINs. Set \code{use_year = "all"} to return
a COIN for all years where data is available. Keep in mind that a COIN represents a single year of data.}

\item{impute_latest}{Logical: if \code{TRUE}, imputes missing data points using most recent value from previous years. If \code{FALSE}
(default) simply extracts the data frame as is. This only works if \code{!is.null(use_year)} and there are previous years of data
available (before \code{use_year}). Currently does not support imputation using future values or interpolation.}
}
\value{
A "COIN" S3 class object (list) formatted to the specifications of COINr. If the input is panel
data and \code{use_year} is set to return multiple years, this function returns a tibble of COINs, indexed
by the year. This latter feature is new and currently under development.
}
\description{
This takes the raw data provided by the user and puts it into a list format (COIN object) that is recognised by COINr.
It also checks whether there are any syntax errors in the data provided. Optionally, you can exclude
or include indicators using the \code{include} and \code{exclude} arguments. Note that if an indicator is specified in
BOTH \code{include} and \code{exclude}, it will be excluded.
}
\details{
A "COIN" is an S3 class which is a structured list of indicator data, metadata, results and methodology which is used
throughout COINr. COINs are a convenient way to store all variables relating to the composite indicator in a single named
object. This keeps the workspace tidy, but also allows fast and concise calls to functions, as well as copying COINs to
introduce methodological variations, and enables complex operations such as global sensitivity analysis (see \code{\link[=sensitivity]{sensitivity()}}.

If panel data is input to this function, it will output a tibble of COINs (see \code{use_year}). This feature
is currently under development and more support will be included for these tibbles of COINs in COINr over
time.

For general information on COINs see the COINr vignette as well as the \href{https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html}{relevant chapter} in the COINr online documentation.

For details on copying, adjusting and comparing COINs see the \href{https://bluefoxr.github.io/COINrDoc/adjustments-and-comparisons.html}{COINr chapter on adjustments and comparisons}.
}
\examples{
# build the ASEM COIN
ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)

}
