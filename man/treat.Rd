% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_treat.R
\name{treat}
\alias{treat}
\title{Treatment of outliers}
\usage{
treat(
  COIN,
  dset = NULL,
  winmax = NULL,
  winchange = NULL,
  deflog = NULL,
  boxlam = NULL,
  t_skew = NULL,
  t_kurt = NULL,
  individual = NULL,
  indiv_only = NULL,
  bypass_all = NULL
)
}
\arguments{
\item{COIN}{The COIN object}

\item{dset}{The data set to treat}

\item{winmax}{The maximum number of points to Winsorise for each indicator. If \code{NA}, will keep Winsorising until skewness and kurtosis thresholds
achieved (but it is likely this will cause errors).}

\item{winchange}{Logical: if \code{TRUE} (default), Winsorisation can change direction from one iteration to the next. Otherwise if \code{FALSE}, no change.}

\item{deflog}{The type of transformation to apply if Winsorisation fails. If \code{"log"}, use simple \code{log(x)} as log transform
(note: indicators containing negative values will be skipped). If \code{"CTlog"}, will do \code{log(x-min(x) + a)}, where \code{a <- 0.01*(max(x)-min(x))}, similar to that used in the COIN Tool.
If \code{"CTlog_orig"}, this is exactly the COIN Tool log transformation, which is \code{log(x-min(x) + 1)}.
If \code{"GIIlog"}, use GII log transformation.
If "\verb{boxcox"}, performs a Box-Cox transformation. In this latter case, you should also specify \code{boxlam}. Finally, if \code{"none"}, will
return the indicator untreated.}

\item{boxlam}{The lambda parameter of the Box-Cox transform.}

\item{t_skew}{Absolute skew threshold (default 2)}

\item{t_kurt}{Kurtosis threshold (default 3.5)}

\item{individual}{A data frame specifying individual treatment for each indicator, with each row corresponding to one indicator to be treated. Columns are:
\itemize{
\item \code{IndCode} The code of the indicator to be treated.
\item \code{Treat} The type of treatment to apply, one of \code{"win"} (Winsorise), \code{"log"} (log), \code{"GIIlog"} (GII log), \code{"CTlog"} (COIN Tool log),
\code{"boxcox"} (Box Cox), or \code{"None"} (no treatment).
\item \code{Winmax} The maximum number of points to Winsorise. Ignored if the corresponding entry in \code{"Treat"} is not \code{"win"}.
\item \code{Thresh} Either \code{NA}, which means that Winsorisation will continue up to \code{winmax} with no checks on skew and kurtosis, or \code{"thresh"},
which uses the skew and kurtosis thresholds specified in \code{t_skew} and \code{t_kurt}.
\item \code{boxlam} Lambda parameter for the Box Cox transformation
}}

\item{indiv_only}{Logical: if \code{TRUE}, only the indicators specified in \code{"individual"} are treated.
If \code{FALSE}, all indicators are treated: any outside of \code{individual} will get default treatment.}

\item{bypass_all}{Logical: if \code{TRUE}, bypasses all data treatment and returns the original data. This
is useful for sensitivity analysis and comparing the effects of turning data treatment on and off.}
}
\value{
If the input is a COIN, outputs an updated COIN with a new treated data set at \code{.$Data$Treated}, as well as
information about the data treatment in \code{.$Analysis$Treated}. Else if the input is a data frame, outputs both the treated
data set and the information about data treatment to a list.
}
\description{
Takes the COIN object and Winsorises indicators where necessary or specified, or reverts to log transform or similar. This is done
one indicator at a time.
}
\details{
Outliers are identified according to skewness and kurtosis thresholds. The algorithm attempts to reduce the absolute skew and
kurtosis by successively Winsorising points up to a specified limit. If this limit is reached, it applies a nonlinear transformation.

The process is detailed in the \href{https://bluefoxr.github.io/COINrDoc/data-treatment.html#data-treatment-in-coinr}{COINr online documentation}.
}
\examples{
# assemble ASEM COIN
ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
# treat raw data set, Winsorise up to a maximum of five points
ASEM <- treat(ASEM, dset = "Raw", winmax = 5)
# inspect what was done
ASEM$Analysis$Treated$TreatSummary
# check whether skew and kurtosis now within limits
ASEM$Analysis$Treated$StatTable$SK.outlier.flag

}
\seealso{
\itemize{
\item \code{\link[=indDash]{indDash()}} Interactive app for checking indicator distributions. Useful for comparing before/after data treatment.
}
}
