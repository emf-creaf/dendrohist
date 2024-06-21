#' Set storage mode of all columns in a data.frame
#'
#' @description
#' Function \code{set_colmode} allows to set the storage mode of all columns in a
#' \code{data.frame} in one step.
#'
#'
#' @param df \code{data.frame}
#' @param cl \code{character} vector with the names of the storage mode for each column
#' in \code{df}. Its length must be the same as the number of column in \code{df}.
#' Accepted strings are "character", "integer", "double" or "numeric" (they are equivalent)
#'  "logical", "complex" and "raw".
#'
#' @details
#' Simple implementation of conversion functions \code{as.character}, \code{as.integer},
#' \code{as.double}, \code{logical}, \code{as.complex} and \code{as.raw}.
#'
#'
#' @return
#' The input \code{df} data.frame with the storage.mode of its columns modified.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a=1:4, b=letters[1:4])
#' print(sapply(df, typeof))
#' print(sapply(set_colmode(df, c("character", "character")), typeof))
#'
set_colmode <- function(df, cl) {

  # Checks.
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))
  stopifnot("Input 'cl' must be a character vector" = is.vector(cl) & is.character(cl))
  stopifnot("Names in 'cl' are not correct" = all(cl %in% c("character", "integer", "double", "numeric", "logical", "complex", "raw")))
  nc <- ncol(df)
  stopifnot("Number of columns in 'df' must match the number of elements in 'cl'" = nc == length(cl))

  for (i in 1:nc) {
    x <- df[, i]
    df[i] <- switch(cl[i],
                    character = as.character(x),
                    integer = as.integer(x),
                    double = as.double(x),
                    numeric = as.numeric(x),
                    logical = as.logical(x),
                    complex = as.complex(x),
                    raw = as.raw(x))
  }

  return(df)

}
