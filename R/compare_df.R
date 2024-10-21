#' Compare two or more data.frames
#'
#' \code{data.frames} are checked whether their column numbers, column names
#' and column formats all match. It also checks for duplicated rows (there shouldn't
#' be any).
#'
#' @param ... comma-separated 'data.frames' to be compared.
#' @param index_duplicated logical, if set to TRUE the output will consist of the indices
#' of the duplicated rows.
#'
#' @return
#' 'TRUE' if the comparisons were successful; FALSE if they were not.
#' If \code{index_duplicated} is TRUE the output will instead be a vector containing
#' the indices of the rows that are duplicated.
#'
#' @details
#' The content of the columns do not need to match; that is, inputs can have
#' different number of rows and contain different numbers/characters/anything.
#'
#' Messsages are printed on screen showing the progress of the comparison:
#' * Inputs are checked for "data.frameness".
#' * Column numbers must match.
#' * Column names must match one by one, if previous conditions succeeds.
#' * Column format must match one by one.
#'
#' @export
#'
#' @examples
#' df1 <- as.data.frame(matrix(runif(100), 20, 5))
#' colnames(df1) <- letters[1:5]
#' df2 <- as.data.frame(matrix(runif(80), 20, 4))
#' colnames(df2) <- letters[2:5]
#'
#' # Success in all but duplicated rows.
#' compare_df(df1, df1, df1)
#'
#' # Failure.
#' compare_df(df1, df2)
#' compare_df(runif(10), df1)
#'
compare_df <- function(..., index_duplicated = FALSE) {


  # Retrieve databases.
  db <- list(...)


  assertthat::assert_that(length(db) >= 2, msg = "Input must have at least 2 data.frames")
  assertthat::assert_that(is.logical(index_duplicated), msg = "Input 'index_duplicated' must be 'TRUE' or 'FALSE'")


  # Function to compare list elements.
  ff <- function(x) {
    z <- TRUE
    for (i in 2:length(x)) {
      if (!identical(x[[1]], x[[i]])) {
        z <- FALSE
        break
      }
    }
    return(z)
  }

  # Checks:

    # Inputs must be data.frame. If not, checks stop here.
    if (all(sapply(db, function(x) inherits(x, "data.frame")))) {
      cli::cli_alert_success("Checking inputs are all 'data.frame'")
    } else {
      cli::cli_alert_danger("Checking inputs are all 'data.frame': failed")
      return(FALSE)
    }

    # Number of columns must match.
    if (length(unique(sapply(db, ncol))) == 1) {
      cli::cli_alert_success("Checking same number of columns")
      flag = TRUE
    } else {
      cli::cli_alert_danger("Checking same number of columns: failed")
      flag <- FALSE
    }

    # Name of columns must match.
    if (flag) {
      if (ff(lapply(db, colnames))) {
        cli::cli_alert_success("Checking same name of columns")
      } else {
        cli::cli_alert_danger("Checking same name of columns: failed")
        flag <- FALSE
      }
    }

    # Format of columns must match.
    if (flag) {
      x <- lapply(db, function(x) sapply(1:ncol(x), function(j) class(x[, j])))
      if (ff(x)) {
        cli::cli_alert_success("Checking same column format")
      } else {
        cli::cli_alert_danger("Checking same column format: failed")
        flag <- FALSE
      }
    }

    # Rows should not be duplicated.
    if (flag) {
      x <- db[[1]]
      for (i in 2:length(db)) x <- rbind(x, db[[i]])
      i <- duplicated(x)
      if (any(i[-1])) {
        cli::cli_alert_danger("Checking rows are not duplicated: failed")
        if (index_duplicated) {
          return(which(i))
        }
        flag <- FALSE
      } else {
        cli::cli_alert_success("Checking no rows are duplicated")
      }


    }

  return(flag)
}
