#' Convert GGMMSS coordinate to decimal number.
#'
#' @description
#' \code{extract_ddmmss} extracts the degrees, minutes and seconds from the input DDMMSS and converts it
#' to a decimal number.
#'
#' @param x character, it must be in the format DDDMMSS. A minus or plus sign is allowed, i.e. +DDDMMSS or
#' -DDDMMSS are accepted (or +/-MMSS, +/-MSS, +/-SS, +/-S).
#' @param test_range logical, if set to TRUE a warning
#' message is issued if the range of the coordinates (either seconds, minutes or degrees) are outside reasonable values.
#' See Details below.
#'
#' @details
#' The coordinates in the input \code{x} must be DDDMMSS, +DDDMMSS or -DDDMMSS.
#' The allowed range for seconds and minutes is [-60, 60].
#' However, we allow for degrees to be in the range [-360, 360], not the usual [-180, 180] for longitude or
#' [-90, 90] for latitude.
#'
#' Strings with more than 8 characters are not allowed. In those strings with 8 characters, the leftmost character
#' must be a plus or minus sign. That is, "+1234556" (123 degrees, 45 minutes and 56 seconds) and "-1234567" are fine,
#' but not "+12345534", "+01234556", "-1234556" or "+01234556", for example.
#'
#' @return
#' The coordinate as a decimal number.
#'
#' @export
#'
#' @examples
#' # Result is 34.36556
#' extract_ddmmss("342156")
#'
#' # Result is -434.3656 with a warning.
#' extract_ddmmss("-4342156")
#'
#' # Same without warning.
#' extract_ddmmss("-4342156", F)
extract_ddmmss <- function(x, test_range = T) {


  # Checks.
  stopifnot("Input 'x' must be a vector of character type" = is.character(x) & is.vector(x))
  nc <- nchar(x)
  stopifnot("There are empty strings" = all(nc > 0))
  stopifnot("There are non-numeric strings" = all(!is.na(suppressWarnings(as.numeric(x)))))


  # Save and remove the sign, if present.
  nx <- length(x)
  i <- substr(x, 1, 1)
  sign <- rep(1, nx)
  sign[i == "-"] <- -1
  i <- which(i %in% c("-", "+"))
  x[i] <- substring(x[i], 2)


  # Once removed, the string must have at least 1 character and a maximum of 7.
  nc <- nchar(x)
  stopifnot("Strings cannot have more than 7 characters (i.e. 7 digits and a '-' or '+' sign)" = all(nc <= 7))


  # Extract values from strings.
  ss <- as.numeric(substring(x, nc-1))
  x <- substr(x, 1, nc-2)
  nc <- nc-2
  mm <- as.numeric(substring(x, nc-1))
  dd <- as.numeric(substr(x, 1, nc-2))


  # NA's are substituted by 00.
  mm[is.na(mm)] <- 0
  dd[is.na(dd)] <- 0


  # Test range.
  if (test_range) {
    if (any(ss > 60)) warning(paste0("\n Range of seconds is not in [-60, 60]"))
    if (any(mm > 60)) warning(paste0("\n Range of minutes is not in [-60, 60]"))
    if (any(dd > 360)) warning(paste0("\n Range of degrees is not in [-360, 360]"))
  }



#
#   for (i in 1:nx) {
#
#     z <- x[i]
#
#     # Seconds.
#     nc <- nchar(z)
#     ss[i] <- as.numeric(substring(z, nc-1))
#     z <- substr(z, 1, nc-2)
#     if (test_range) {
#       if (ss[i] > 60) message(paste0("\n Range of seconds is not in [-60, 60] in ", z, "\n\n"))
#     }
#
#     # Minutes.
#     nc <- nchar(z)
#     mm[i] <- as.numeric(substring(z, nc-1))
#     z <- substr(z, 1, nc-2)
#     if (test_range) {
#       if (mm[i] > 60) message(paste0("\n Range of minutes is not in [-60, 60] in ", z, "\n\n"))
#     }
#
#     # All that is left is degrees.
#     dd[[i]] <- as.numeric(z)
#     if (test_range) {
#       if (dd[[i]] > 360) message(paste0("\n Range of minutes is not in [-360, 360] in ", z, "\n\n"))
#     }
#
#     # Convert to decimal degrees.
#     y[i] <- sign[i] * (g + m/60 + s/3600)
#
#   }
#
#
# #
# #
# #   if (test_range) {
# #     if (abs(g) > 360) message(paste0("\n Range of degrees is not in [-360, 360] in ", xx, "\n\n"))
# #   }
#
#



  return(sign * (dd + mm/60 + ss/3600))
}
