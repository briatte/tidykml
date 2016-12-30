#' Read Lines out of a KML file.
#' 
#' @param x A KML source. See \link{kml_read}.
#' @param ns The name of the namespace to extract from: defaults to \code{"d1"}.
#' @param verbose Whether to report invalid coordinates and/or altitudes below
#' sea level; defaults to \code{TRUE}. See \link{kml_coords}.
#' @param ... Arguments passed to \link[xml2:read_xml]{read_xml}. 
#' See \link{kml_read}.
#' @return A \link[tibble:tibble]{tibble} containing the \code{folder} (layer), 
#' \code{name}, \code{description}, \code{styleUrl} and geographic coordinates 
#' (\code{longitude}, \code{latitude} and \code{altitude}) of the \emph{first}
#' LineString contained within each Placemark element of the KML source. 
#' Other Placemark elements will be ignored.
#' If there are no LineStrings in the KML file, the function returns 
#' \code{NULL}.
#' @note The function only extracts the \strong{first} LineString out of each
#' Placemark element. As a result, multi-LineStrings built into <MultiGeometry> 
#' elements are \emph{not} fully supported: only the first LineString will be 
#' present in the results.
#' @examples
#' # demo data: U.S. Civil War map
#' # see ?states for details
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_lines(f)
#' @seealso Google Developers. KML Reference: <Point> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#linestring}
#' @importFrom dplyr bind_rows data_frame
#' @importFrom stringr %>% str_c str_split
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @export
kml_lines <- function(x, ns = "d1", verbose = TRUE, ...) {
  
  x <- kml_read(x, ...)
  x <- kml_folders(x, ns)
  
  # case: no layers
  if (!length(x)) {
    return(NULL)
  }
  
  x <- lapply(x, function(x) {
    
    f <- kml_element(x, "name", ns)
    x <- kml_placemarks(x, "LineString", ns)
    
    # case: no placemarks
    if (!length(x)) {
      return(NULL)
    }
    
    data_frame(
      folder = f,
      name = kml_element(x, "name", ns),
      description = kml_element(x, "description", ns),
      styleUrl = kml_element(x, "styleUrl", ns),
      coordinates = kml_element(x, str_c(
        "LineString/",
        ns,
        ":coordinates"),
        ns) %>%
        str_split("\\s+") %>%
        unlist
    )
    
  }) %>%
    bind_rows
  
  if (!nrow(x)) {
    
    return(NULL)
    
  } else {
    
    x$longitude <- kml_coords(x$coordinates, 1, verbose)
    x$latitude  <- kml_coords(x$coordinates, 2, verbose)
    x$altitude  <- kml_coords(x$coordinates, 3, verbose)
    x$coordinates <- NULL
    
    return(x)
    
  }
  
}