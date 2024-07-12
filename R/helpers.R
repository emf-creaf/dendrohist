to_numeric <- function(x) {
  i <- which(x == "")
  x <- as.numeric(x)
  x[i] <- 0
  return(x)
}

select_coord <- function(df, x_y, cs) {

  # Usual checks.
  stopifnot("Input 'df' must be a data.frame" = inherits(df, "data.frame"))
  stopifnot("Input character 'x_y' must be equal to 'x' or 'y'" = is.character(x_y) & any(x_y %in% c("x", "y")))
  stopifnot("Input 'cs' must be 'UTM', 'UTM30', 'WGS80', 'ED50' or 'ETRS89'" = any(cs %in% c("UTM", "UTM30", "WGS84", "ED50", "ETRS89")))


  # Choose reference system.
  m <- switch(cs,
              UTM = "utm",
              UTM30 = "utm30",
              ED50 = "long",
              WGS84 = "longwgs84",
              ETRS89 = "etrs89")

  browser()
  if (any(cs %in% c("UTM", "UTM30", "ETRS89"))) {
    b$x <- b[, paste0("x", m)]
    b$y <- b[, paste0("y", m)]
  } else {
    x <- sapply(b[, paste0("x", m)], function(z) extract_gms(as.character(z)))
    y <- sapply(b[, paste0("y", m)], function(z) extract_gms(as.character(z)))
  }


  return(xy)

}

