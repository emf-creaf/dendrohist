#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Correct data.frames.
#' df1 <- data.frame(x = runif(10), y = sample(letters[1:10]), z = sample(c(F, T), 10, replace = T))
#' df2<- data.frame(x = runif(10), y = sample(letters[1:10]), z = sample(c(F, T), 10, replace = T))
#' df3 <- data.frame(x = runif(10), y = sample(letters[1:10]), z = sample(c(F, T), 10, replace = T))
#'
merge_flood_DB <- function(...) {


  # Checks.
  db <- list(...)
  stopifnot("Inputs must be of class 'data.frame'" = all(sapply(db, function(x) inherits(x, "data.frame"))))


  # Number of columns must match.
  stopifnot("Number of columns in input data.frames must be the same" = length(unique(sapply(db, ncol))) == 1)

  # Name of columns must match.
  colnam <- sapply(db, colnames)
  i <- apply(colnam, 1, function(x) length(unique(x)))


  # Format of columns must match.
  colfor <- sapply(db, function(x) sapply(1:ncol(x), function(j) class(x[, j])))
  j <- apply(colfor, 1, function(x) length(unique(x)))


browser()

}
