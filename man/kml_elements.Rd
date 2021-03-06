% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internals.R
\name{kml_elements}
\alias{kml_elements}
\title{Extract multiple KML elements from a Placemark.}
\usage{
kml_elements(x, element, ns = "d1")
}
\arguments{
\item{x}{A nodeset of Placemarks.}

\item{element}{The name of the element to extract, e.g. \code{"name"}.}

\item{ns}{The name of the namespace to extract from; defaults to \code{"d1"}.}
}
\value{
A character vector holding the text of the element.
Missing values, i.e. empty elements, will be returned as \code{NA} values.
}
\description{
Extract multiple KML elements from a Placemark.
}
\seealso{
Google Developers. KML Reference: <Placemark> Element.
\url{https://developers.google.com/kml/documentation/kmlreference#placemark}
}
\keyword{internal}

