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
#'  "logical", "complex", "raw" and "date".
#'
#' @details
#' Simple implementation of conversion functions \code{as.character}, \code{as.integer},
#' \code{as.double}, \code{logical}, \code{as.complex}, \code{as.raw} and \code{as.Date}.
#'
#' NB! It is not possible to specify which \code{format} to apply in the "date" conversion.
#' That may yield unwanted \code{Date} results if the input format is not the standard
#' %Y-%m-%d. Rather than converting all columns in one sweep, keep the column with the
#' date as a character and convert it afterwards (see Example below).#'
#'
#' @return
#' The input \code{df} data.frame with the storage.mode of its columns modified.
#'
#' @export
#'
#' @examples
#' # Simple example.
#' df <- data.frame(a=1:4, b=letters[1:4])
#' print(sapply(df, typeof))
#' print(sapply(set_colmode(df, c("character", "character")), typeof))
#'
#' #More complicated.
#' df <-
#'
#'
set_colmode <- function(df, cl) {

  # Checks.
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))
  stopifnot("Input 'cl' must be a character vector" = is.vector(cl) & is.character(cl))
  cl <- tolower(cl)
  stopifnot("Names in 'cl' are not correct" = all(cl %in% c("character", "integer", "double", "numeric", "logical", "complex", "raw", "date")))
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
                    raw = as.raw(x),
                    date = as.Date(x))
  }

  return(df)

}
