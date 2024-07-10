fetch_stations <- function(str_url = NULL, rs = "WGS84") {


  # Which reference system?
  rs <- match.arg(crs, c("WGS84", "ED50", "UTM", "UTM30"))


  # URL by default, corresponding to the Ebro basin.
  if (is.null(str_url)) {
    str_url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020/EBRO/"
  }


  # Get data.
  x <- read.csv2(paste0(str_url, "estaf.csv"))


  # Get long-lat coordinates right.
  if (any(rs %in% c("WGS84", "ED50"))) {
    if (rs == "WGS84") {
      xc <- x$longws84
      yc <- x$latwgs84
    } else {
      xc <- x$long
      yc <- x$lat
    }
    nx <- nchar(xc)
    ny <- nchar(yc)

    s <- extract_gms(xc, "s")
    m <- extract_gms(xc, "m")
    g <- extract_gms(xc, "g")



  }

  # Add geometry.
  x <- switch(rs,
              "WGS84" = sf::st_as_sf(x, coords = c("longwgs84", "latwgs84")),
              "ED50" = sf::st_as_sf(x, coords = c("long", "lat")),
              "UTM" = sf::st_as_sf(x, coords = c("xutm", "yutm")),
              "UTM30" = sf::st_as_sf(x, coords = c("xutm30", "yutm30")))


  # Add the coordinate reference system.
  x <- sf::st_set_crs(x, 4326)


}
