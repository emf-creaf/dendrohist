#' Find chunks of NA values
#'
#' @details
#' Find the indices of chunks of NA values in input vector.
#'
#' @param x numeric o character vector
#'
#' @return a \code{list} whose length matches the number of NA segments
#' found in \code{x}. Each \code{list} element contains the indices of the
#' NA values for every chunk/lump/segment. Extracting those chunks is as easy as using
#' those indices to get the corresponding \code{x} values.
#'
#' If there are no NA's, function returns an empty list.
#'
#' @description
#' A chunk is defined as a group of contiguous NA values.
#' Single NA values, with no contiguous NA's, are also chunks of length 1.
#' This function returns a list with as many elements as NA chunks present in the data.
#'
#' @export
#'
#' @examples
#' # A numeric vector. It can also be character or logical (not shown).
#' x <- runif(100)
#' x[1] <- NA
#' x[30:33] <- NA
#' x[56] <- NaN
#' x[78:79] <- NA
#' x[100] <- NA
#' chunksNA <- chunksNA(x)
#'
#' # Extract those parts of x that do NOT have NA.
#' y <- ifelse(is.na(x), 1, NA)
#' chunksnotNA <- chunksNA(y)
#'
#' # Find chunks of consecutive 9's in first 20000 digits of number Pi (try other digits).
#' # Are these chunks stochastic? If so, they must agree with a theoretical
#' # probability density function.
#' # Load 20000 first digits of pi from table as a vector.
#' x <- read.csv("http://oeis.org/A000796/b000796.txt", header=FALSE, sep=" ")[,2]
#' nx <- sum(x == 9)
#' x[x == 9] <- NA
#' y <- chunksNA(x)
#' z <- table(sapply(y, length))
#' plot(c(1, 6), c(0, 1), xlab = "Length", ylab = "Probability", type = "n")
#' points(1:length(z), z/nx, pch = 1, cex = 2)
#' # Probability of no 9's on both sides of a digit.
#' ps <- (9/10)^2
#' # Now, given a 9 is present, we compute the probability of other 9's but no 9's on both sides.
#' p <- ps * (1/10)^(0:(length(z)-1))
#' points(1:length(z), p, type = "l", lwd = 2)
#' legend("topright", c("Observed", "Theoretical"), pch = c(1, NA), lty = c(NA, 1), lwd = c(NA, 2))
#'
chunksNA <- function(x) {

  stopifnot("Input 'x' must be a vector" = is.vector(x))


  # First find NA's.
  xNA <- is.na(x)
  xnonNA <- !xNA


  # If length(x) = 1 no need to go through the algorithm below (which needs length(x) > 1).
  y <- list()
  if (length(x) == 1) {
    if (xNA) y[[1]] <- 1
    return(y)
  }


  # If there are no NA's, return an empty list.
  if (sum(xNA) == 0) return(y)


  # Initialize counter. If first point contains NA, initialize list y.
  icount <- 0
  if (xNA[1]) {
    y[[1]] <- 1
    icount <- icount + 1
  }


  # Main algorithm.
  for (i in 2:length(x)) {
    if (xNA[i]) {
      if (xnonNA[i-1]) {
        icount <- icount + 1
        y[[icount]] <- i
      } else {
        y[[icount]] <- c(y[[icount]], i)
      }
    }
  }

  return(y)
}
