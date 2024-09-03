#' EPSG codes for CEH coordinates
#'
#' @description
#' It translates the "utm30", "etrs89", "wgs84" or "ed50" strings into EPSG codes as used in the coordinate
#' systems at the gauging stations of the CEH.
#'
#'
#' @param x character vector with a short string description of the coordinate system.
#' It can contain the string "utm30", "etrs89", "wgs84" or "ed50". Upper/Lower case letters are
#' also accepted.
#'
#' @return
#' Corresponding EPSG codes in a named vector.
#'
#' @details
#' Simple implementation of \code{switch}.
#'
#'
#' @export
#'
#' @examples
#' epsg_ceh(c("ed50", "wgs84"))
epsg_ceh <- function(x) {


  # Checks.
  stopifnot("Input must be a vector with at least a single element" = is.vector(x))
  stopifnot("Input must be of character type" = is.character(x))
  x <- tolower(x)
  stopifnot("Wrong input" = any(c("utm30", "etrs89", "wgs84", "ed50") %in% x))

  # Choose EPSG code CEH data.
  z <- sapply(x, function(y) switch(y,
              utm30 = 32630,
              etrs89 = 25830,
              wgs84 = 4326,
              ed50 = 4230))


  return(z)
}
