# tidykml

The `tidykml` package reads selected elements and values from [KML][google-kml] files, 
such as those produced by [Google My Maps][google-my-maps], and puts them into [tidy data
frames][tdf]. The resulting data frames are intended for use with packages like
[`dplyr`][cran-dplyr] and [`ggplot2`][cran-ggplot2].

## Motivation

The goal of `tidykml` is to make KML files usable for data wrangling and 
visualization in as few steps as possible. Several R packages can import KML 
files, but these packages do not offer a straightforward way to use their 
results with either `dplyr` or `ggplot2`.

The reason for `tidykml` to exist will go away when packages like [`ggmap`][cran-ggmap], [`rgdal`][cran-rgdal] and
[`sf`][cran-sf] implement easy ways to produce tidy data frames from KML data, or to
fortify KML data into objects that can be passed to `ggplot2`.

## Limitations

- The `tidykml` package was tested with only a limited number of KML files, all
  of which came from [Google My Maps][google-my-maps].
- The `tidykml` package does not fully support `<MultiGeometry>` elements, such 
  as multi-polygons, and will only handle their _first_ element.

Due to these limitations, `tidykml` lives on GitHub but will probably never show up on CRAN.

## Installation

```R
devtools::install_github("briatte/tidykml")
library(tidykml)
```

## Example

The data used in this example is a [map of the U.S. Civil War][map-states] featured on Google My Maps (see `?states` for details).

The `tidykml` package contains functions to return the Points,
Polygons or LineStrings of a KML file:

```R
library(dplyr)
f <- system.file("extdata", "states.kml.zip", package = "tidykml")
kml_polygons(f) %>%
    glimpse
```

The results are always returned in the following form:

```
Observations: 9,930
Variables: 7
$ folder      <chr> "States (status in 1863)", "States (status in 1863)", "S...
$ name        <chr> "Ohio", "Ohio", "Ohio", "Ohio", "Ohio", "Ohio", "Ohio", ...
$ description <chr> "description: type: Union state<br>type: Union state", "...
$ styleUrl    <chr> "#poly-3F5BA9-1-196", "#poly-3F5BA9-1-196", "#poly-3F5BA...
$ longitude   <dbl> -82.21486, -82.34138, -82.54884, -82.71695, -82.90893, -...
$ latitude    <dbl> 41.46419, 41.43150, 41.39134, 41.45053, 41.42947, 41.456...
$ altitude    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
```

These results are easy to pass to [`ggplot2`][cran-ggplot2]:

```R
library(ggplot2)
kml_polygons(f) %>%
    ggplot(aes(longitude, latitude, group = name)) +
      geom_polygon(color = "white") +
      coord_map("albers", at0 = 45.5, lat1 = 29.5)
```

![](http://i.imgur.com/d9lyU6r.png)

These results are also easy to pass to [`ggmap`][ggmap]:

```R
library(ggmap)
m <- get_map(kml_bounds(f), source = "osm")
ggmap(m) +
  geom_polygon(data = kml_polygons(f) %>%
                 mutate(type = gsub("(.*)<br>type: (.*)", "\\2", description)),
               aes(longitude, latitude, group = name, fill = type),
               color = "white", alpha = 0.5) +
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
```

![](http://i.imgur.com/3Xgox6x.jpg)

The final map also shows the location of major U.S. civil war battles:

```R
ggmap(m) +
  geom_polygon(data = kml_polygons(f) %>%
                 mutate(type = gsub("(.*)<br>type: (.*)", "\\2", description)),
               aes(longitude, latitude, group = name, fill = type),
               color = "white", alpha = 0.5) +
  geom_point(data = kml_points(f),
             aes(longitude, latitude),
             color = "darkred", size = 6, alpha = 0.5) +
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
```

![](http://i.imgur.com/hNSfpdi.jpg)

## Data

In addition to the example map used above, the package also contains a [map of non-Hispanic gangs in South Los Angeles][map-gangs], created by Instagram user [@la\_hood\_maps][map-gangs-source] (see `?gangs` for details).

```R
f <- system.file("extdata", "gangs.kml.zip", package = "tidykml")
m <- get_map(kml_bounds(f), source = "osm")
ggmap(m) +
  geom_polygon(data = kml_polygons(f),
               aes(longitude, latitude, group = name, fill = folder),
               color = "grey25", alpha = 0.75) +
  scale_fill_brewer("", palette = "Set3",
                    guide = guide_legend(override.aes = list(color = NA))) +
  labs(title = "Non-Hispanic Gangs in South Los Angeles (2016)",
       caption = paste("Source: instagram.com/la_hood_maps",
                       "(accessed 30 December 2016)."),
       x = NULL, y = NULL) +
  theme(legend.position = "right",
        legend.justification = c(0, 1),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        axis.text = element_blank(),
        axis.ticks = element_blank())
```

![](http://i.imgur.com/UIUJCVz.png)

[cran-dplyr]: https://cran.r-project.org/package=dplyr "Package 'dplyr' (CRAN)"
[cran-ggmap]: https://cran.r-project.org/package=ggmap "Package 'ggmap' (CRAN)"
[cran-ggplot2]: https://cran.r-project.org/package=ggplot2 "Package 'ggplot2' (CRAN)"
[cran-rgdal]: https://cran.r-project.org/package=rgdal "Package 'rgdal' (CRAN)"
[cran-sf]: https://cran.r-project.org/package=sf "Package 'sf' (CRAN)"
[cran-tibble]: https://cran.r-project.org/package=tibble "Package 'tibble' (CRAN)"
[google-kml]: https://developers.google.com/kml/documentation/kmlreference "KML Reference (Google Developers)"
[google-my-maps]: https://en.wikipedia.org/wiki/Google_My_Maps "Google My Maps (Wikipedia)"
[map-gangs-source]: https://www.instagram.com/la_hood_maps/ "La Hood Maps (Instagram)"
[map-gangs]: https://goo.gl/7Ar1Aa "Gangs of Los Angeles (2016) (Google My Maps)"
[map-states]: https://goo.gl/rezvty "US Civil Wars (Google My Maps)"
