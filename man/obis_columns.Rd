% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obis.R
\name{obis_columns}
\alias{obis_columns}
\title{Retrieve a list of known column names and \code{\link[readr]{cols}} declarations}
\usage{
obis_columns(what = "all")
}
\arguments{
\item{what}{character, by default all values are returned, but specify a subset with
a character vector of desired columns (variables).  All other columns will be set to
\code{\link[readr]{col_skip}}}
}
\value{
names list
}
\description{
Retrieve a list of known column names and \code{\link[readr]{cols}} declarations
}
