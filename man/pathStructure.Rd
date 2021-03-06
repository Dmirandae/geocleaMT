% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathStructure.R
\name{pathStructure}
\alias{pathStructure}
\title{Folder structure}
\usage{
pathStructure(path.dir = NULL, group = NULL)
}
\arguments{
\item{path.dir}{Vector of characters. Path to the folders' structure for all data/results.}

\item{group}{Vector of characters. Name or names of group; create a 
subdirectory for each group into each main directory. See details.}
}
\value{
Created folders into the assigned path.
}
\description{
Create the file structure for the protocol proposed 
R-Alarcon & Miranda-Esquivel (submitted) [1].
}
\details{
A group is a high taxonomic level such as Amphibia, Reptilia, Mammalia, etc.
}
\note{
See:
[1] R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
cleaning geographical data from electronic biodatabases.
}
\author{
R-Alarcon Viviana and Miranda-Esquivel Daniel R.
}
\references{
[1] R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
cleaning geographical data from electronic biodatabases.
}

