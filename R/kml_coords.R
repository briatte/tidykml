#' Extract KML coordinates (longitude, latitude or altitude).
#' 
#' @param x A character vector of KML coordinates, of the form
#' \code{"longitude,latitude[,altitude]"}.
#' @param coord Which coordinate to extract: either \code{1} (longitude), 
#' \code{2} (latitude) or \code{3} (altitude). The function also accepts
#'  \code{"lon"}, \code{"lat"} and \code{"alt"}.
#' @param verbose Whether to report invalid coordinates and/or negative
#' altitudes (below sea level); defaults to \code{TRUE}. See 'Note'.
#' @return A numeric vector.
#' 
#' If \code{coord} has been set to return altitude and there are no altitude 
#' values in the KML coordinates, the function returns a numeric vector of
#' \code{NA} values of the same length as \code{x}.
#' @note KML coordinates are \code{lon,lat[,alt]} tuples.
#' 
#' Invalid coordinates are longitudes outside of [-180, 180] (angular distance 
#' in degrees, relative to the Prime Meridian) and latitudes outside of 
#' [-90, 90] (angular distance in degrees, relative to the Equator).
#' 
#' Altitude is the distance from the earth's surface, in meters. Depending on
#' the altitude mode used in the KML source, altitude might be computed in 
#' absolute terms (relative to sea level), or from the actual ground elevation
#' of a particular location.
#' @seealso Google Developers. KML Reference: <Camera> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#camera}
#' @examples 
#' # longitude
#' kml_coords("11.0,22.0,-99.0", 1)
#' # latitude
#' kml_coords("11.0,22.0,-99.0", 2)
#' # altitude
#' kml_coords("11.0,22.0,-99.0", "alt")
#' kml_coords("11.0,22.0,-99.0", "alt", verbose = FALSE)
#' @importFrom stringr %>% str_c str_count str_replace
#' @export
kml_coords <- function(x, coord, verbose = TRUE) {
  
  # case: coord is an abbreviation
  if (is.character(coord)) {
    coord <- which(c("lon", "lat", "alt") == coord)
  }
  
  stopifnot(length(coord) == 1 && coord %in% 1:3)
  
  n <- unique(str_count(x, ","))
  
  # case: irregular format
  if (length(n) > 1) {
    stop("Irregular coordinates format.")
  }
  
  # case: invalid format
  if (!n %in% 1:2) {
    stop("Invalid coordinates format.")
  }
  
  # case: altitude was requested but is not present
  if (coord == 3 && n == 1) {
    return(rep(NA_real_, length(x)))
  }
  
  n <- str_c(rep("(.*)", n + 1), collapse = ",")
  x <- str_replace(x, n, str_c("\\", coord)) %>%
    as.numeric

  if (verbose) {
    
    if (coord == 1 && any(abs(x) > 180)) {
      message("Some longitudes are not contained within [-180, 180].")
    }
    
    if (coord == 2 && any(abs(x) > 90)) {
      message("Some latitudes are not contained within [-90, 90].")
    }
    
    if (coord == 3 && any(x < 0)) {
      message("Some altitudes are negative.")
    }
    
  }
  
  return(x)
  
}