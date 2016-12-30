#' Find the elements of a KML file.
#' 
#' @param x A KML source. See \link{kml_read}.
#' @param ns The name of the namespace to extract from: defaults to \code{"d1"}.
#' @return A named numeric vector of five elements corresponding to the number
#' of Folders, Placemarks, LineStrings, Points and Polygons in the source.
#' @examples
#' # demo data: U.S. Civil War map
#' # see ?states for details
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_info(f)
#' @importFrom stringr %>% str_c
#' @importFrom xml2 xml_find_all
#' @export
kml_info <- function(x, ns = "d1") {
  
  x <- kml_read(x)

  c(
    sapply(c("Folder", "Placemark"), function(y) {
      xml_find_all(x, str_c("//", ns, ":", y)) %>%
        length
    }),
    sapply(c("LineString", "Point", "Polygon"), function(y) {
      xml_find_all(x, str_c("//", ns, ":", y, "//", ns, ":coordinates")) %>%
        length
    })
  )
  
}
