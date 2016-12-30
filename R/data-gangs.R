#' Map of Non-Hispanic Gangs in South Los Angeles (2016)
#'
#' This map shows the non-Hispanic gangs present in South Los Angeles as of
#' December 2016. 
#' 
#' The map comes from Google My Maps website and has been used to illustrate the
#' Wikipedia entry for the service.
#' 
#' The precise identity of the author of the map could not be determined.
#' @source Anonymous. Gangs of Los Angeles (2016). \url{https://goo.gl/7Ar1Aa} 
#' (Google My Maps, accessed 30 December 2016).
#' @seealso LA Hood Maps. \emph{Instagram}.
#' \url{https://www.instagram.com/la_hood_maps/} (accessed 30 December 2016).
#' @docType data
#' @name gangs
#' @format Zipped KML file.
#' @examples
#' f <- system.file("extdata", "gangs.kml.zip", package = "tidykml")
#' kml_polygons(f)
NULL