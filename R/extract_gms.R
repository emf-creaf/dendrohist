#' Convert GGMMSS coordinate to decimal number.
#'
#' @description
#' \code{extract_gms} extracts the degrees, minutes and seconds from the input GGMMSS and converts it
#' to a decimal number.
#'
#' @param x character, it must be in the format GGGMMSS. A minus or plus sign is allowed, i.e. +GGGMMSS or
#' -GGGMMSS are accepted (or +/-MMSS, +/-MSS, +/-SS, +/-S).
#' @param test_range logical, if set to TRUE the range of the coordinates is tested and a warning
#' message is issued if they are outside reasonable values. See Details below.
#'
#' @details
#' The coordinates in the input \code{x} must be GGGMMSS, +GGGMMSS or -GGGMMSS.
#' The allowed range for seconds and minutes is [-60, 60].
#' However, we allow for degrees to be in the range [-360, 360], not the usual [-180, 180].
#'
#' @return
#' The coordinate as a decimal number. If the
#' @export
#'
#' @examples
#' # Result is 34.36556
#' extract_gms("342156")
#'
#' # Result is -434.3656 with a warning.
#' extract_gms("-4342156")
#'
#' # Same without warning.
#' extract_gms("-4342156", F)
extract_gms <- function(x, test_range = T) {


  # Checks.
  stopifnot("Input 'x' must be a string" = is.character(x))
  stopifnot("Length of 'x' must be = 1" = length(x) == 1)


  # Seconds.
  nx <- nchar(x)
  sign <- 1
  if (nx > 0) {
    s <- as.numeric(substr(x, nx-1, nx))
    if (s < 0) {
      s <- abs(s)
      sign <- -1
    }
    x <- substr(x, 1, nx-2)
    nx <- nx-2
    # If only one character is left, check if it is a minus/plus sign.
    # If it is so, empty 'x'.
    if (nx == 1) {
      if (any(x %in% c("-", "+"))) {
        if (x == "-") sign <- -1
        nx <- 0
      }
    }

    # Test range.
    if (test_range) {
      if (abs(s) > 60) warning("Range of seconds is not in [-60, 60]")
    }
  }


  # Minutes.
  if (nx > 0) {
    m <- as.numeric(substr(x, nx-1, nx))
    if (m < 0) {
      m <- abs(m)
      sign <- -1
    }
    x <- substr(x, 1, nx-2)
    nx <- nx-2

    # If only one character is left, check if it is a minus/plus sign.
    # If it is so, empty 'x'.
    if (nx == 1) {
      if (any(x %in% c("-", "+"))) {
        if (x == "-") sign <- -1
        nx <- 0
      }
    }

    # Test range.
    if (test_range) {
      if (abs(m) > 60) warning("Range of minutes is not in [-60, 60]")
    }

  } else {
    m <- 0
  }


  # Degrees.
  g <- ifelse(nx > 0, as.numeric(substr(x, 1, nx)), 0)
  if (g < 0) {
    g <- abs(g)
    sign <- -1
  }

  if (test_range) {
    if (abs(g) > 360) warning("Range of degrees is not in [-360, 360]")
  }


  # Convert to decimal degrees.
  y <- sign * (g + m/60 + s/3600)


  return(y)
}
