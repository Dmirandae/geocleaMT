% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordToCountry.R
\name{coordToCountry}
\alias{coordToCountry}
\title{From coordinate in decimal degree format to country name (English)}
\usage{
coordToCountry(coordinatesTable = NULL)
}
\arguments{
\item{coordinatesTable}{Data Frame class. A data frame with at least two columns
called \code{decimalLatitude} and \code{'decimalLongitude'}.}
}
\value{
A table with three columns (decimalLatitude, decimalLongitude, country)
}
\description{
Give a coordinate in decimal degree format to assign the
corresponding country name in English.
}
\details{
If the coordinate is outside the continents, this can not be assigned and
will it will return a \code{'NaN'}.
}
\note{
See:
R-Alarcon V. and Miranda-Esquivel DR.(submmited) geocleaMT: An R package to
cleaning geographical data from electronic biodatabases.
}
\author{
R-Alarcon Viviana and Miranda-Esquivel Daniel R.
}
\seealso{
\code{\link{coordToISO}}
}

