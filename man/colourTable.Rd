% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_resultsdash.R
\name{colourTable}
\alias{colourTable}
\title{Conditionally formatted table}
\usage{
colourTable(
  df,
  freeze1 = TRUE,
  sortcol = NULL,
  sortorder = "desc",
  searchable = TRUE,
  pagesize = 10,
  cell_colours = NULL,
  reverse_colours = FALSE
)
}
\arguments{
\item{df}{A data frame to be displayed as a table.}

\item{freeze1}{If \code{TRUE} (default), freezes the first column. This may be for example the unit name or code.}

\item{sortcol}{A column name to sort the table by. Defaults to first numerical column. Set to \code{"none"} to disable.}

\item{sortorder}{Either \code{"desc"} for sorted column to be sorted from high to low (default) or \code{"asc"} for the opposite.}

\item{searchable}{If \code{TRUE}, includes a search box}

\item{pagesize}{The number of rows to display on each page.}

\item{cell_colours}{A character vector of at least two colour codes (e.g. Hex codes) to use for the colour palette. Should be in
order of low to high values. Defaults to a simple green palette of \code{c("#eefff4", "#358554")}. See \code{\link[grDevices:colorRamp]{grDevices::colorRamp()}} for more info.}

\item{reverse_colours}{If \code{TRUE}, reverses the colour map - useful for rank tables where lowest numbers mean high scores.}
}
\value{
An interactive table generated by reactable.
}
\description{
Given a data frame, generates a conditionally-formatted html table using reactable. This function is used by
\code{\link[=iplotTable]{iplotTable()}}. It is a quick wrapper for \link[reactable:reactable]{reactable::reactable}.
}
\examples{
# some random data
df <-  as.data.frame(matrix(runif(12), 3, 4))
# a names column
df <- cbind(Rnames = letters[1:3], df)
# round it
df <- roundDF(df)
# make a table
colourTable(df)

}
\seealso{
\itemize{
\item \code{\link[=iplotTable]{iplotTable()}} Interactive table of indicator data (from a COIN)
}
}
