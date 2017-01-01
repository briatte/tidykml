tidykml 0.1.2 (2017-01-01)
--------------------------

CHANGES

* Option to read data directly from Google My Maps.

* Option to force kml_polygons to fuse multiple-geometry polygons together. This
option is experimental, and is highly likely to return erroneous geometries.

* More detailed internal functions.


tidykml 0.1.1 (2016-12-31)
--------------------------

This version has been tested against GADM (gadm.org) files -- with very mixed 
success: maps with inner boundaries will not render appropriately, and detailed
maps will take a long time to process.

FIXES

* Deal with whitespace in <coordinates> elements.

CHANGES

* Support for files with no <Folder> element(s).

* Support for KMZ sources.


tidykml 0.1.0 (2016-12-30)
--------------------------

First release.

This version has been tested against KML files from Google My Maps.
