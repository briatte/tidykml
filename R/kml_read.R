#' Read a KML file.
#' 
#' @param x A string, a connection, or a raw vector.
#' All inputs accepted by \link[xml2:read_xml]{read_xml} are also accepted, as 
#' long as they are valid KML. This means that URLs and local compressed files,
#' such as \code{.zip} files, are also supported. 
#' See \link[xml2:read_xml]{read_xml} for details.
#' @param ... Arguments passed to \link[xml2:read_xml]{read_xml}, such as 
#' \code{encoding} or \code{base_url}.
#' See \link[xml2:read_xml]{read_xml} for details.
#' @return A nodeset.
#' @examples
#' # demo data: U.S. Civil War map
#' # see ?states for details
#' f <- system.file("extdata", "states.kml.zip", package = "tidykml")
#' kml_read(f)
#' @seealso Google Developers. KML Reference.
#' \url{https://developers.google.com/kml/documentation/kmlreference}
#' @importFrom stringr %>% str_detect
#' @importFrom xml2 read_xml xml_ns
#' @export
kml_read <- function(x, ...) {
  
  if (is.character(x)) {
    x <- read_xml(x, ...)
  }
  
  y <- xml_ns(x)
  if (!length(y) || !str_detect(y, "/kml/")) {
    stop("Source does not seem to be a valid KML document.")
  }
  
  return(x)
  
}
