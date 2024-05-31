#' Substitute curly quotes by straight ones.
#'
#' @description
#' \code{curly_quotes} performs a substitution of curly quotation marks (left or right,
#' single or double) by straight quotation marks.
#'
#' @param x character vector.
#' @param single logical, if set to TRUE (default) any double curly quotation mark
#' will be substituted by a single straight quotation mark. Otherwise, a double curly
#' quotation mark will be subsituted by a double straight quotation mark.
#'
#' @return
#' A character vector of the same length as \code{x} with curly quotation marks
#' substituted.
#'
#' @details
#' Simple implementation of the \code{gsub} command.
#'
#' @export
#'
#' @examples
#' x <- c("Straight quote'", "Single straight '‘and right curly quotes ",
#' "Double “ ”and ” single ‘curly ’ ''quotes")
#' print(rbind(original = x, single = curly_quotes(x), double = curly_quotes(x, F)))
curly_quotes <- function(x, single = T) {


  # Checks.
  stopifnot("Input 'x' must be a character vector" = is.vector(x) & is.character(x))
  nx <- length(x)
  stopifnot("Length of 'x' must be > 0" = nx > 0)

  # Replacement.
  x <- gsub("‘", "'", x) # Curled to the right.
  x <- gsub("’", "'", x) # Curled to the left.
  if (single) {
    x <- gsub("“", "'", x) # Single curled to the right.
    x <- gsub("”", "'", x) # Single curled to the left.
  } else {
    x <- gsub("“", "''", x) # Double curled to the right.
    x <- gsub("”", "''", x) # Double curled to the left.
  }


  return(x)
}
