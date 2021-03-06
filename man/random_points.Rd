% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mpa.R
\name{random_points}
\alias{random_points}
\title{Generate a sf object of random points within and around a set of polygons.}
\usage{
random_points(x = read_mpa("Cuba"), buffer = 0.1, n = 1000)
}
\arguments{
\item{x}{sf object of polygons}

\item{buffer}{numeric See \code{\link[sf]{st_buffer}}}

\item{n}{integer, the number of points}
}
\value{
a sf object of n points
}
\description{
Polygons are expanded by \code{buffer}, and then samples are drawn from
those.  See \code{\link[sf]{st_buffer}}
}
