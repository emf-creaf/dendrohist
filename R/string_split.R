#' Split string at character position
#'
#' @description
#' \code{slash_split} splits a string into two at the location of the first split character.
#'
#' @param x character vector.
#' @param split character, can be "/" or "-".
#' @param trim logical, if set to TRUE all leading and trailing whitespaces are
#' removed *after* the split.

#'
#' @return
#' A \code{data.frame} with two named columns "left" and "right", containing
#' the left and right side of the string that has been splitted at the split
#' location. If no slash is present, "left" will be equal to the original
#' string and "right" will be NA.
#'
#' If the split character is located at the first character, "left" will be an empty
#' character and "right" will contain the string at the right side of the
#' slash. The opposite will happen if the split character is at the end of the string.
#'
#' The number of roes of the output \code{data.frame} is equal to the length
#' of vector \code{x}.
#'
#' @details
#' The implementation does not take into account more than one split character.
#' If there is none, no splitting is applied.
#'
#' @export
#'
#' @examples
#' x <- c("This is left/ and right", "/ Slash at start",
#' "Slash at the end /", "No slash at - all", "or is it - a dash")
#' print(string_split(x, "/"))
#' print(string_split(x, "-"))
string_split <- function(x, split = "/", trim = T) {


  # Checks.
  stopifnot("Input 'x' must be a character vector" = is.vector(x) & is.character(x))
  nx <- length(x)
  stopifnot("Length of 'x' must be > 0" = nx > 0)
  stopifnot("Value of 'split' must be a single character" = nchar(split) == 1)
  stopifnot("Value of 'split' can only be '/' or '-'" = any(split %in% c("/", "-")))


  # The string is split at the first "\". Not further slashes are considered.
  sl <- grepl(split, x)


  # If there are no slashes, return right away.
  if (sum(sl) == 0) {
    df <- data.frame(left = x, right = NA)

  } else {
    df <- data.frame(left = character(), right = character())
    for (i in 1:nx) {
      if (sl[i]) {
        nc <- nchar(x[i])
        j <- regexpr(split, x[i])[1]
        if (j == 1) {
          xleft <- ""
          xright <- substr(x[i], 2, nc)
        } else if (j == nc) {
          xleft <- substr(x[i], 1, nc-1)
          xright <- ""
        } else {
          xleft <- substr(x[i], 1, j-1)
          xright <- substr(x[i], j+1, nc)
        }


        # Need a trim?
        if (trim) {
          xleft <- trimws(xleft)
          xright <- trimws(xright)
        }

      } else {
        xleft <- ifelse(trim, trimws(x[i]), x[i])
        xright <- NA
      }

      df <- rbind(df, data.frame(left = xleft, right = xright))
    }
  }


  return(df)
}
