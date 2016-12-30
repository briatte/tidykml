#' Map of U.S. Civil War
#'
#' This map shows the major battles of the U.S. Civil War, the states engaged in
#' the conflict and their status as of 1863, and the itinerary of Union General 
#' William T. Sherman's forces from Atlanta to Savannah, Georgia, in 1864.
#' 
#' The map comes from Google My Maps website and has been used to illustrate the
#' Wikipedia entry for the service.
#' 
#' The precise identity of the author of the map could not be determined.
#' @source Anonymous. US Civil War. \url{https://goo.gl/rezvty}
#' (Google My Maps, accessed 30 December 2016).
#' @seealso Wikipedia. Google My Maps. \emph{Wikipedia, The Free Encyclopedia}.
#' \url{https://en.wikipedia.org/wiki/Google_My_Maps} (revision 732328417,
#' accessed 30 December 2016).
#' @docType data
#' @name states
#' @format Zipped KML file.
#' @examples
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_points(f)
NULL