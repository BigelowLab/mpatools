% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obis.R
\name{fetch_obis}
\alias{fetch_obis}
\title{Given a simple features tibble (POLYGON or MUTLIPOLYGON), retrieve OBIS observation records.}
\usage{
fetch_obis(
  x = read_mpa("Cuba"),
  combine = TRUE,
  policy = c("convex hull", "strict")[1]
)
}
\arguments{
\item{x}{MPA sf object such as for a country}

\item{combine}{logical, if TRUE then combine all MPAs into one convex polygon. This reduces the
network from many small ones to a single large one.  Use this to reduce the number of calls
to OBIS, the trade off is that the request may many records that are subsequently filtered
out if \code{policy} is 'strict'.}

\item{policy}{character either "convex hull" or "strict"
We fetch the convex hull to simplify the fetch, but that may allow some observations that are outside MPAs.
If "convex hull" then we allow these to be returned.  If "strict" then we filter for those observations that are in
the MPAs (in mean within or on the boundary).}
}
\value{
tibble of OBIS observations, possibly empty
}
\description{
Given a simple features tibble (POLYGON or MUTLIPOLYGON), retrieve OBIS observation records.
}
