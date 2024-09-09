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


  # Several checks.
  db <- list(...)

    # Inputs must be data.frames
    stopifnot("Inputs must be of class 'data.frame'" = all(sapply(db, function(x) inherits(x, "data.frame"))))

    # Number of columns must match.
    stopifnot("Number of columns in input data.frames must be the same" = DescTools::AllIdentical(lapply(db, colnames)))

    # Name of columns must match.
    stopifnot("Name of columns do not match" = DescTools::AllIdentical(lapply(db, colnames)))

    # Format of columns must match.
    colfor <- lapply(db, function(x) sapply(1:ncol(x), function(j) class(x[, j])))
    stopifnot("Format of columns do not match" = DescTools::AllIdentical(colfor))



browser()

}
