#' Check for leap year
#'
#' @description
#' \code{is_leap_year} checks input (which can be \code{numeric} or \code{Date}) for leap year.
#'
#' @param year a \code{Date} or \code{numeric} vector.
#'
#' @return
#' \code{logical} vector of the same length as the input informing whether or not each element
#' corresponds to a leap year.
#'
#' @details
#' First version prepared with Google Gemini. Minor changes afterwards.
#'
#' @export
#'
#' @examples
#' x <- c("02/27/37", "02/27/46", "01/14/92", "02/28/96", "02/01/99")
#' is_leap_year(as.Date(x, "%m/%d/%y"))
is_leap_year <- function(year) {


  # Checks.
  class_year <- inherits(year, "Date")
  stopifnot("Input 'year' must be an 'Date' object or numeric" = class_year | is.numeric(year))


  # Extract year if it's a Date object.
  if (class_year) year <- as.integer(format(year,'%Y'))


  # Apply leap year logic in a single chain
  return(year %% 400 == 0 | (year %% 4 == 0 & year %% 100 != 0))

}
