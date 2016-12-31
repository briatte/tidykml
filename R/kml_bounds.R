#' Find the boundaries of a KML file.
#' 
#' @param x A KML source. See \link{kml_read}. \code{x} can also be a data frame
#' with two numeric variables named \code{longitude} and \code{latitude}.
#' @param ns The name of the namespace to extract from: defaults to \code{"d1"}.
#' @param verbose Whether to report invalid coordinates and/or altitudes below
#' sea level; defaults to \code{TRUE}. See \link{kml_coords}.
#' @return A named numeric vector of four elements corresponding to the 
#' left, bottom, right and top values of the bounding box; 'left' and 'right' 
#' are the minimal and maximal longitudes; 'bottom' and 'top' are the minimal 
#' and maximal latitudes.
#' @examples
#' # demo data: U.S. Civil War map
#' # see ?states for details
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_bounds(f)
#' @seealso \link{kml_coords}
#' @importFrom stringr %>% str_c str_length str_split
#' @importFrom xml2 xml_find_all xml_text
#' @export
kml_bounds <- function(x, ns = "d1", verbose = TRUE) {
  
  if ("data.frame" %in% class(x)) {
    
    stopifnot(c("longitude", "latitude") %in% names(x))
    
    lon <- unique(x$longitude)
    lat <- unique(x$latitude)
    
  } else {
    
    x <- kml_read(x) %>%
      xml_find_all(str_c("//", ns, ":coordinates")) %>%
      xml_text %>%
      str_split("\\s+") %>% # deal with Polygon coordinates
      unlist
    
    # drop empty <coordinates>
    x <- x[ str_length(x) > 0 ]
    
    lon <- unique(kml_coords(x, 1, verbose))
    lat <- unique(kml_coords(x, 2, verbose))
    
  }

  x <- c(
    left   = min(lon),
    bottom = min(lat),
    right  = max(lon),
    top    = max(lat)
  )
  
  return(x)
  
}