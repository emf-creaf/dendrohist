#' Title
#'
#' @param julian_date
#'
#' @returns
#'
#' @details
#' If 'julian_date' is before ***, it is assumed to be from the Julian calendar. If not, it is
#' assumed to be a Gregorian date and the output is identical to the input.
#'
#' @export
#'
#' @examples
date_conversion <- function(julian_date) {

  stopifnot("Input 'julian_date' must be a Date object" = inherits(julian_date, "Date"))

  day_zero <- timeDate::timeDate("4713/01/01 BCE", format = "%Y/%m/%d")

  2440588



}
