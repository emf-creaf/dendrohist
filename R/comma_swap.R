#' Remove comma and swap words.
#'
#' @description
#' A short description...
#'
#' @param x character vector.
#' @param trim logical, if set to TRUE all leading and trailing whitespaces are
#' removed *after* the swap.
#'
#' @details
#' Input strings are assumed to have a single comma ',' dividing the sentence.
#' That is common in databases of places, like e.g. "Rosario, El" (a municipal term
#' on the Canary Islands). In this example, we would like it to be written as
#' "El Rosario", with the article "El" preceding the name "Rosario".
#'
#' This function will do the swap only for the first comma. If there are no commas,
#' the output string will be exactly the input string. Also, any extra comma
#' will be considered as text. E.g. "Madrid, Spain, Europe" would be
#' converted into "Spain, Europe Madrid".
#'
#' @return
#' A character vector of the same length as \code{x}.
#' @export
#'
#' @examples
#' x <- c(",", " ,", ", ", " , ", "Rosario, El", "Madrid capital", "Madrid, Spain, Europe")
#' print(rbind(x, comma_swap(x)))
#'
#' # Notice the difference.
#' print(rbind(x, comma_swap(x, F)))
comma_swap <- function(x, trim = T) {


  # Checks.
  stopifnot("Input 'x' must be a character vector" = is.vector(x) & is.character(x))
  nx <- length(x)
  stopifnot("Length of 'x' must be > 0" = nx > 0)


  # Swapping, if needed. nchar must be > 2; otherwise, it won't swap.
  y <- character(nx)
  for (i in 1:nx) {
    nc <- nchar(x[i])
    if (nc > 2) {
      j <- regexpr(",", x[i])[1]
      if (j == -1) {
        y[i] <- x[i]
      } else if (j == 1){
        y[i] <- substring(x, 2)
      } else if (j == nc) {
        y[i] <- substring(x, 1, nc - 1)
      } else {
        x1 <- substring(x[i], 1, j-1)
        x2 <- substring(x[i], j+1, nc)
        y[i] <- paste0(x2, " ", x1)
      }
    } else {
      y[i] <- x[i]
    }
  }


  # Need a trim?
  if (trim) y <- trimws(y)


  return(y)
}
