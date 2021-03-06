% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stripAttr.R
\name{stripAttr}
\alias{stripAttr}
\title{Remove all attributes from a column of a data.frame/data.table}
\usage{
stripAttr(col)
}
\arguments{
\item{col}{the column of a data.frame or data.table to be cleaned of attributes}
}
\value{
a column cleaned from all attributes except for
names, row.names, and class
}
\description{
Removes attributes of a column as they are often unnecessary. Found on stackoverflow.
}
