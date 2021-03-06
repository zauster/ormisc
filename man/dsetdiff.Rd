% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsetdiff.R
\name{dsetdiff}
\alias{dsetdiff}
\title{Double setdiff}
\usage{
dsetdiff(setA, setB, throwError = TRUE)
}
\arguments{
\item{setA}{first set, typically a character vector}

\item{setB}{second set}

\item{throwError}{boolean, should an error be thrown if differences exist? Default is TRUE}
}
\value{
list of differences and an error
}
\description{
Double setdiff
}
\details{
Do a double setdiff and (default) throw an error.
}
\author{
Oliver Reiter
}
