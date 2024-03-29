% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_resultstable.R
\name{getUnitReport}
\alias{getUnitReport}
\title{Generate unit report}
\usage{
getUnitReport(
  COIN,
  usel,
  out_type = ".html",
  outdir = NULL,
  rmd_template = NULL
)
}
\arguments{
\item{COIN}{A COIN}

\item{usel}{A selected unit code, or a character vector of unit codes (for multiple reports).}

\item{out_type}{A string specifying the output type. Can be either \code{".docx"} (Word), \code{".pdf"} or \code{".html"}. IMPORTANT: if the
template includes interactive plots (e.g. the \code{iplot()} functions from COINr), writing to \code{.docx} or \code{.pdf} will not work
\emph{unless} you have installed the webshot package. To do this, run:
\code{install.packages("webshot")}
\code{webshot::install_phantomjs()}}

\item{outdir}{Character string specifying the output directory (defaults to current working directory).}

\item{rmd_template}{A character string specifying the full file path to an R Markdown template which is used to generate the report. If this is not specified,
defaults to COINr's inbuilt template example.}
}
\value{
Markdown document rendered to HTML, pdf or Word. This function requires Pandoc to be installed. If Pandoc is not found,
then it returns a warning and a printed message (string).
}
\description{
Generates a scorecard for a given unit using an R Markdown template.
}
\details{
Most likely you will want to customise the template which can be found in the COINr installed package directory under \verb{/UnitReport}.
Currently, a few examples are given, such as some charts and basic summary statistics.

This function will render the unit report to either pdf, html or word doc. As mentioned below, if you have HTML widgets
such as interactive plotly plots, or COINr \code{iplot()} functions, you will need to install the webshot package to be able to
render to pdf or word formats.

To customise the template, copy the \code{.rmd} template found in \verb{/UnitReport} and alter it, then point the \code{rmd_template} argument to
your new template.

Note that this function is particularly useful for generating a large number of reports, e.g. we can generate reports for
all units at once using a \code{for} loop, \link[purrr:map]{purrr::map} or \code{\link[=apply]{apply()}} or similar.
}
\examples{
# build ASEM COIN up to aggregation
ASEM <- build_ASEM()
# Generate a unit report for NZ
# This is written to the temporary directory to avoid polluting other directories
# during automated testing.
# It will be deleted at the end of the R session.
# Normally you would set the directory to somewhere else to save the resulting files
getUnitReport(ASEM, usel = "NZL", out_type = ".html", outdir = tempdir())
# You can find this file in the temporary directory:
print(tempdir())
# We will now delete the file to keep things tidy in testing
unlink(paste0(tempdir(),"\\\\NZL_report.html"))

}
