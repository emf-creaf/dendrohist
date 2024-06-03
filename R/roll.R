#' Shift a vector.
#'
#' @description
#' \code{roll} shifts a vector right or left with a wrap-up effect.
#'
#' @param x a vector of length > 1.
#' @param n an integer stating the amount by which \code{x} will be shifted or rolled.
#'
#' @return
#' A shifted vector of the same length as \code{x}.
#'
#' @details
#' Found at \code{https://stackoverflow.com/questions/26997586/shifting-a-vector}.
#' A positive \code{n} moves the vector to the right (as if "pushing" from the left)
#' so that elements at the right tip of the vector wrap up from the left.
#'
#' @export
#'
#' @examples
#' print(roll(letters[1:5], 2))
roll <- function(x, n) {

  # Checks.
  stopifnot("Input 'x' must be a vector" = is.vector(x))
  stopifnot("Input 'n' must be an integer" = round(n) == n)


  # If length is 1 no need to do anything.
  nx <- length(x)
  if (nx == 1) {
    warning("Length of 'x' is 1")
    return(x)
  }


  # Algorithm.
  n <- -n
  x <- x[(0:(nx-1) + n) %% nx + 1]


  return(x)
}
