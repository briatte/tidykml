context("Test tidykml")

test_that("bounds, info, lines, points, polygons work", {
  
  # demo data: U.S. Civil War map
  # see ?states for details
  f <- system.file("extdata", "states.kml.zip", package = "tidykml")
  
  expect_is( kml_info(f)     , class = "integer")
  expect_is( kml_bounds(f)   , class = "numeric")
  expect_is( kml_bounds(kml_lines(f))   , class = "numeric")
  
  expect_is( kml_lines(f)    , class = "data.frame")
  expect_is( kml_points(f)   , class = "data.frame")
  expect_is( kml_polygons(f) , class = "data.frame")
  
})

test_that("lines, points, polygons return NULLs when required", {
      
  # demo data: U.S. Civil War map
  # see ?states for details
  f <- system.file("extdata", "gangs.kml.zip", package = "tidykml")
  
  expect_null( kml_lines(f)  )
  expect_null( kml_points(f) )
  
  # NO POLYGONS
  
  f <- '<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
        <name>foo</name>
        <description>bar</description>
    		<Folder>
    			<name>empty</name>
        </Folder>
      </Document>
    </kml>'
  
  expect_null( kml_polygons(f) )
  
  # EMPTY POLYGON

  f <- '<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
        <name>foo</name>
        <description>bar</description>
        <Folder>
          <name>empty</name>
          <Placemark>
            <name>empty polygon placemark</name>
            <Polygon>
              <name>empty polygon</name>
            </Polygon>
          </Placemark>
        </Folder>
      </Document>
    </kml>'
  
  expect_null( kml_polygons(f) )
  
  # NULL RETURNS WHEN THERE IS NOTHING IN THE FILE
  
  f <- '<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
        <name>foo</name>
        <description>bar</description>
      </Document>
    </kml>'
  
  expect_null( kml_lines(f)    )
  expect_null( kml_points(f)   )
  expect_null( kml_polygons(f) )
  
  # EMPTY POLYGON INSIDE A DOCUMENT WITH NO FOLDER
  
  f <- '<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
        <name>foo</name>
        <description>bar</description>
        <Placemark>
          <name>empty polygon placemark</name>
          <Polygon>
            <name>empty polygon</name>
          </Polygon>
        </Placemark>
      </Document>
    </kml>'
  
  expect_null( kml_polygons(f) )
  
})

test_that("kml_read throws an error when the source is not KML", {
  
  expect_error(kml_read("<foo></foo>"))
  
})

test_that("kml_coords works", {
  
  # longitude
  expect_equal(kml_coords("11.0,22.0,-99.0", 1), 11)
  
  # latitude
  expect_equal(kml_coords("11.0,22.0,-99.0", 2), 22)
  
  # altitude
  expect_message(kml_coords("11.0,22.0,-99.0", "alt"))
  expect_equal(kml_coords("11.0,22.0,-99.0", "alt", verbose = FALSE), -99)
  
  # CASES NOT COVERED BY EXAMPLES
  
  # irregular coordinates
  expect_error(kml_coords(c("11.0,22.0,-99.0", "11.0,22.0"), 1))
  
  # invalid coordinates
  expect_error(kml_coords("nah", 1))
  
  # no altitude
  expect_equal(kml_coords("11.0,22.0", "alt"), NA_real_)
  
  # weird coords
  expect_message(kml_coords("999.0,999.0", 1))
  expect_message(kml_coords("999.0,999.0", 2))

})
