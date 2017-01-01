#' Read Polygons out of a KML file.
#' 
#' @inheritParams kml_points
#' @param fuse Whether to fuse multi-polygons into a single element; defaults
#' to \code{FALSE}. Experimental. Might not return nice things.
#' @return A \link[tibble:tibble]{tibble} containing the \code{folder} (layer), 
#' \code{name}, \code{description}, \code{styleUrl} and geographic coordinates 
#' (\code{longitude}, \code{latitude} and \code{altitude}) of the \emph{first}
#' Polygon contained within each Placemark element of the KML source. 
#' Other Placemark elements will be ignored.
#' 
#' If there are no Polygons in the KML source, the function returns \code{NULL}.
#' If there are no Folders in the KML source, the \code{folder} variable will be
#' filled with \code{NA}.
#' @note The function only extracts the outer bounds of Polygon elements, and it
#' only extracts the \strong{first} Polygon out of each Placemark element. As a
#' result, multi-polygons built into <MultiGeometry> elements are \emph{not} 
#' fully supported: only the first Polygon will be present in the results.
#' @references Google Developers. KML Reference: <Polygon> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#polygon}
#' @examples
#' # demo data: U.S. Civil War map
#' # see ?states for details
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_polygons(f)
#' @importFrom dplyr bind_rows data_frame
#' @importFrom stringr %>% str_c str_split
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @export
kml_polygons <- function(x, ns = "d1", verbose = TRUE, fuse = FALSE, ...) {
  
  x <- kml_read(x, ...)
  y <- kml_folders(x, ns)

  if (!length(y)) {
    
    # case: no folders
    x <- xml_find_all(x, str_c("//", ns, ":Document"))
    
  } else {
    
    x <- y
    y <- length(y)
    
  }
  
  x <- lapply(x, function(x) {
    
    f <- kml_element(x, "name", ns)
    x <- kml_placemarks(x, "Polygon", ns)
    
    # case: no placemarks
    if (!length(x)) {
      return(NULL)
    }
    
    lapply(x, function(x, folder = f) {
      
      #
      # Multi-polygons are tough cookies. The solution taken here is the simple
      # way out: just take the first one into account, and assume that the rest
      # of the polygons are insignificant islands. Not optimal, I know.
      #
      geometry <- ifelse(
        xml_find_all(x, str_c(ns, ":MultiGeometry")) %>%
          length, # detect multi-polygons
        str_c("MultiGeometry/", ns, ":Polygon/"),
        "Polygon/"
      )
      
      data_frame(
        folder,
        name = kml_element(x, "name"),
        description = kml_element(x, "description"),
        styleUrl = kml_element(x, "styleUrl"),
        # if the Placemark contains several Polygons in a MultiGeometry, the 
        # next lines will only find the coordinates of the first Polygon (see
        # longer note above)
        coordinates = do.call(
          ifelse(fuse, "kml_elements", "kml_element"),
          args = list(x = x, element = str_c(
            geometry,
            ns,
            ":outerBoundaryIs/",
            ns,
            ":LinearRing/",
            ns,
            ":coordinates"),
            ns)
        ) %>%
          str_split("\\s+") %>%
          unlist
      )
      
    }) %>%
      bind_rows
    
  }) %>%
    bind_rows
  
  return(kml_finalize(x, folders = length(y) > 0, verbose))

}
