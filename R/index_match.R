#' Matching
#'
#' @param x character vector containing the patterns to be matched in \code{y}.
#' @param y character vector where matches are sought.
#' @param max.distance numeric vector with different measures of approximativeness.
#' See \code{\link[base]{agrep}} for details. If a value \code{max.distance=0} is included,
#' the first match is carried out with the \code{\link[base]{match}} function,
#' much faster.
#' @param verbose logical, if set to TRUE a progress bar, as well as some other info text
#' is printed on screen.
#'
#' @return
#' A \code{list} with two elements with the same length as the length of \code{x}.
#' First column, labelled 'index', contains the indices in \code{y} of
#' every element of \code{x}. Second column, labelled 'distance.value', contains
#' the value of \code{max.distance} at which the corresponding 'index was found.
#' When match fails, both columns have an NA value.
#'
#' @details
#' If \code{max.distance} is very high, every string in \code{x} will match almost
#' anything in \code{y} because we will have allowed for lots of insertions, deletions
#' and substitutions. In those cases, we keep the first position only, for coherence
#' with the \code{\link[base]{match}} function.
#'
#' @export
#'
#' @examples
#' # Simple.
#' i <- index_match(letters[1:10], letters[4:15], max.distance = 0)
#'
#' # If max.distance is increased, even those different strings match.
#' i <- index_match(letters[1:10], letters[4:15], max.distance = 2)
#'
#' # More complicated.
#' x <- c("january", "feb")
#' y <- month.name
#'
#' # Check 'distance.value' column with 'ignore.case' is set.
#' print(index_match(x, y, max.distance = c(0, .1),ignore.case = F,verbose = T))
#' print(index_match(x, y, max.distance = c(0, .1),ignore.case = T,verbose = T))
#'
index_match <- function(x, y, max.distance = 0, ignore.case = F, verbose = F) {

  # Checks.
  stopifnot("Inputs 'x' and 'y' must be vectors" = is.vector(x) & is.vector(y))
  stopifnot("Input 'max.distance' must be >= 0" = all(max.distance >= 0))
  nx <- length(x)


  # Sort max.distance in increasing order.
  max.distance <- sort(max.distance)


  # First, exact matches are sought, if set max.distance = 0.
  distance.value <- rep(NA, nx)
  if (max.distance[1] == 0) {
    if (ignore.case) {
      index <- match(tolower(x), tolower(y))
    } else {
      index <- match(x, y)
    }
    distance.value[!is.na(index)] <- 0
    max.distance <- max.distance[-1]

  } else {
    index <- rep(NA, nx)
  }


  # Are there matches unaccounted for?
  nind <- sum(is.na(index)) == 0
  if (nind) {
    if (verbose) {
       cat("\n All matches are exact!\n")
    }

  } else {

    # There are more max.distance values to try out.
    if (length(max.distance) > 0) {

      # Progress-bar initial setup.
      if (verbose) {
        cat("\n Trying approximate matching...\n\n")

        pb <- utils::txtProgressBar(min = 0,
                                    max = nx,
                                    style = 3,
                                    width = 50,
                                    char = "=")
      }

      # Loop along elements in x.
      for (i in 1:nx) {

        if (verbose) setTxtProgressBar(pb, i)

        # Has an exact match not been found before?
        if (is.na(index[i])) {

          # Try increasing values of 'max.distance'. First value is not used.
          for (j in max.distance) {

            # Approximate matching.
            k <- agrep(x[i], y, max.distance = j, ignore.case = ignore.case)

            # Found!
            if (length(k) > 0) {
              index[i] <- k[1]
              distance.value[i] <- j
              break
            }
          }
        }
      }
    }
  }
  if (verbose) cat("\n")


  return(data.frame(index = index, distance.value = distance.value))

}
