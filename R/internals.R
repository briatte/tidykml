#' Extract a single KML element from a Placemark.
#' 
#' @param x A nodeset of Placemarks.
#' @param element The name of the element to extract, e.g. \code{"name"}.
#' @param ns The name of the namespace to extract from; defaults to \code{"d1"}.
#' @return A character vector holding the text of the element.
#' Missing values, i.e. empty elements, will be returned as \code{NA} values.
#' @seealso Google Developers. KML Reference: <Placemark> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#placemark}
#' @importFrom xml2 xml_find_first xml_text
#' @importFrom stringr %>% str_c
#' @keywords internal
kml_element <- function(x, element, ns = "d1") {
  
  xml_find_first(x, str_c(ns, ":", element)) %>%
    xml_text
  
}

# #' Extract multiple KML elements from a Placemark.
# #' 
# #' @inheritParams kml_element
# #' @return A character vector holding the text of the element.
# #' Missing values, i.e. empty elements, will be returned as \code{NA} values.
# #' @seealso Google Developers. KML Reference: <Placemark> Element.
# #' \url{https://developers.google.com/kml/documentation/kmlreference#placemark}
# #' @importFrom xml2 xml_find_first xml_text
# #' @importFrom stringr %>% str_c
# #' @keywords internal
# kml_elements <- function(x, element, ns = "d1") {
#   
#   xml_find_first(x, str_c(ns, ":", element)) %>%
#     xml_text
#   
# }

#' Finalize a KML tidy data frame
#' 
#' Reads the coordinates out of the \code{coordinates} variable, checks them,
#' and returns the data.
#' @param x The KML data frame to tidy.
#' @param folders The number of folders in the data frame.
#' @param verbose Whether to report invalid coordinates and/or negative
#' altitudes (below sea level); defaults to \code{TRUE}.
#' @importFrom stringr str_length
#' @keywords internal
kml_finalize <- function(x, folders, verbose = TRUE) {
  
  stopifnot(is.data.frame(x))
  
  if (!nrow(x)) {
    
    return(NULL)
    
  } else {
    
    # case: no folders
    if (!folders) {
      x$folder <- NA
    }

    # remove elements with no <coordinates>
    x <- x[ !is.na(x$coordinates), ]

    # drop blank lines around <coordinates>
    x <- x[ which(str_length(x$coordinates) > 0), ]
    
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
  
}

#' Extract KML Folders.
#' 
#' @param x An XML document.
#' @param ns The name of the namespace to extract from; defaults to \code{"d1"}.
#' @return A nodeset of Folders.
#' @seealso Google Developers. KML Reference: <Folder> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#folder}
#' @importFrom xml2 xml_find_all xml_name
#' @importFrom stringr str_c
#' @keywords internal
kml_folders <- function(x, ns = "d1") {

  xml_find_all(x, str_c("//", ns, ":Folder"))
  
}

#' Extract KML Placemarks containing a specific Geometry.
#' 
#' @param x A nodeset of Folders.
#' @param geometry The name of the Geometry to subset on, e.g. \code{"Point"}.
#' @param ns The name of the namespace to extract from; defaults to \code{"d1"}.
#' @return A nodeset of Placemarks.
#' @seealso Google Developers. KML Reference: <Placemark> Element.
#' \url{https://developers.google.com/kml/documentation/kmlreference#placemark}
#' @importFrom xml2 xml_find_all
#' @importFrom stringr str_c
#' @keywords internal
kml_placemarks <- function(x, geometry, ns = "d1") {
  
  x <- xml_find_all(x, str_c(ns, ":Placemark//", ns, ":", geometry, "/.."))
  n <- xml_name(x)
  
  if ("MultiGeometry" %in% n) {
    
    n <- which(n == "MultiGeometry")
    x[ n ] <- xml_find_all(x[ n ], "..")
    
  }
  
  return(x)
  
}
