% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countryToCentroid.R
\name{countryToCentroid}
\alias{countryToCentroid}
\title{From country name to centroid}
\usage{
countryToCentroid(country = NULL)
}
\arguments{
\item{country}{data.frame class. a vector with country names in English. See
details.}
}
\value{
A data frame object with the centroid assigned to each country.
}
\description{
From country name in English to the centroid (Coordinate) of the polygon that
represents this country.
}
\details{
To see the  valid names in English the user can run the command
data(countryNames).
}
\note{
See:
R-Alarcon V, Miranda-Esquivel DR. (submitted) geocleaMT: An R package to cleaning
geographical data from electronic biodatabases.
}
\author{
R-Alarcon Viviana and Miranda-Esquivel Daniel R.
}
\seealso{
\code{\link{countryToISO}}
}

