#' Pattern/Value matching
#'
#' @description
#' \code{rmatch} returns a list of matches of its first argument in its second.
#'
#' @param pattern character vector of regular expressions to be matched in 'text'.
#' @param text a character vector where matches are sought.
#' @param regexpr logical, if set to FALSE (default) \link[base]{match} will be used; otherwise,
#' \link[base]{regexpr} will be implemented (this choice implies that partial matches are also allowed).
#'
#' @return
#' A \code{list} object with the same length as \code{pattern}. Each element of the output consists of a
#' vector with the indices of the matches of the corresponding element of 'pattern' in vector 'text'.
#'
#' @details
#' The search when \code{regexpr=F} (the default) is actually implemented with \link[base]{%in%}.
#'
#' @export
#'
#' @examples
#' x <- c("NA", "asdf", "aaa", "ppNA")
#' y <- c("ASDF", "asdf", "ghhjjh", "aaa", "sdgfdgfhj", "NA", "aaa", "Pasdf")
#'
#' # Notice the differences.
#' print(rmatch(x, y))
#' print(rmatch(x, y, T))
#'
#' print(rmatch(y, x))
#' print(rmatch(y, x, T))
#'
rmatch <- function(pattern, text, regexpr = F) {

  # Checks.
  stopifnot("Inputs 'pattern' and 'text' must be character vectors" =
              is.vector(pattern) & is.character(pattern) & is.vector(text) & is.character(text))
  stopifnot("Input 'regexpr' must be TRUE or FALSE" = is.logical(regexpr))


  # Matching.
  if (regexpr) {
    x <- lapply(pattern, function(q) which(regexpr(q, text) != -1))
  } else {
    x <- lapply(pattern, function(q) which(text %in% q))
  }


  return(x)
}
