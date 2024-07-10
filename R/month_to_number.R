#' Convert English name of month to number
#'
#' @description
#' \code{month_to_number} converts a vector of English month names into numbers, i.e.
#' 'January' will be 1, 'February' will be 2 and so on.
#'
#' @param x string vector containing the names of months.
#' @param tolower logical, if set to TRUE all names are translated to lower case.
#' @param abbreviated logical, if set to TRUE \code{month_to_number} will additionally
#' look for abbreviated names (e.g. "Jan" instead of "January"; see \code{month.abb} for
#' accepted abbreviations).
#'
#' @return
#' A numeric vector of the same length as 'x'.
#'
#' @details
#' Simple implementation of \code{match}.
#'
#' @export
#'
#' @examples
#' x <- c("Asd","October", "January", "Dec")
#' print(month_to_number(x))
month_to_number <- function(x, to_lower = T, abbreviated = T) {


  # Checks.
  stopifnot("Input 'x' must be of 'character' type" = is.character(x))


  # Lower-case text.
  if (to_lower) {
    x <- tolower(x)
    month.name <- tolower(month.name)
  }


  # Loop up names of months.
  i <-  match(x, month.name)
  if (abbreviated) {
    j <- which(is.na(i))

    # Is necessary to look up the abbreviated name?
    if (length(j) > 0) {
      if (to_lower) month.abb <- tolower(month.abb)
      k <- match(x[j], month.abb)
      i[j] <- k
    }
  }


  return(i)

}
