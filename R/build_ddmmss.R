#' Make ddmmss/dddmmss strings from numeric coordinates
#'
#' @description
#' \code{build_ddmmss} builds character string with coordinates of the type +/-ddmmss, or +/-dddmmss
#' from coordinates in numeric form,
#' where dd/ddd, mm and ss stand for degrees, minutes and seconds, respectively.
#'
#' @param x \code{numeric} numeric vector containing the coordinates to convert into string.
#'
#' @return
#' A \code{character} vector of the same length as \code{x}.
#' Strings will have a minimum length of 6 characters ("ddmmss" case), without
#' the sign, and a maximum length of 8 characters, including the sign ("-dddmmss" case).
#'
#' @details
#' If the input is negative, the output will also be negative.
#' That would be equivalent to a -ddmmss/-dddmmsss case.
#'
#' The output string will always have 6 or 7 characters (ddmmss or dddmmss).
#' If the input number is negative, the output will then have 7 ot 8 characters
#' (-ddmmss or -dddmmss).
#'
#' @export
#'
#' @examples
#' x <- c("-25005", "2560032", "25000")
#' y <- extract_ddmmss(x)
#'
#' # Next is exactly as 'x', but with an extra zero for degrees.
#' z <- build_ddmmss(y)
build_ddmmss <- function(x) {


  # Checks.
  stopifnot("Input must be a numeric vector" = is.numeric(x) & is.vector(x))


  # To be used below.
  f <- function(q) ifelse(q < 10, paste0("0", q), as.character(q))


  # Get the sign as a character and make x positive.
  s <- ifelse(x < 0, "-", "")
  x <- abs(x)


  # Get degrees, minutes and seconds. The latter are rounded, not truncated.
  tx <- trunc(x)
  dd <- ifelse(tx == 0, "00", f(tx))
  x <- (x - tx) * 60
  mm <- trunc(x)
  ss <- round((x - mm) * 60)


  # Correct truncation issues.
  se <- ss == 60
  mm <- ifelse(se, mm + 1, mm)
  ss <- ifelse(se, 0, ss)


  return(paste0(s, dd, f(mm), f(ss)))
}
