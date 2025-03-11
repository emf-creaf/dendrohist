#' Generate a sample of correlated X-Y gaussian points.
#'
#' @description
#' \code{corr2dpoints} generates a correlated set of X-Y pairs.
#'
#' @param n integer number of points
#' @param meanx mean of the x set.
#' @param meany mean of the y set.
#' @param sdx standard deviation of the x set.
#' @param sdy standard deviation of the y set.
#' @param corr correlation coefficient between x and y.
#'
#' @returns
#' A data.frame with two columns x-y containing the two datasets.
#'
#' @details
#' Notice that the slope of the relationship between X and Y is given by the
#' relationship slope=sdy/sdx*corr. Thus, by choosing sdx and sdy appropriately
#' we can generate datasets with the desired linear correlation and slope.
#'
#' @export
#'
#' @examples
#'
#' # Same correlation and slope.
#' df <- corr2dpoints(10000, corr = .79)
#' r <- lm(y~x, df)
#' plot(df, pch = 16, cex = .1, xlim = c(-4, 4), ylim = c(-4, 4))
#' points(df$x, predict(r), pch = 16, cex = .1, col = "red")
#' print(cor(df)[1,2])
#' print(coef(r)[2])
#' points(df$x, predict(r), pch = 16, cex = .1, col = "red")
#'
#'
#' # Slope is approximately twice the correlation coefficient.
#' df <- corr2dpoints(1000, corr = .79, sdx = 1, sdy = 2)
#' r <- lm(y~x, df)
#' plot(df, xlim = c(-8, 8), ylim = c(-8, 8))
#' points(df$x, predict(r), pch = 16, cex = .1, col = "red")
#' print(cor(df)[1,2])
#' print(coef(r)[2])
#' print(atan(coef(lm(y~x, df))[2]) * 180/pi)
#'

corr2dpoints <- function(n = 100, meanx = 0, meany = 0, sdx = 1, sdy = 1, corr = 0) {

  # Checks.
  stopifnot("Input 'n' must be an integer number" = round(n) == n)
  stopifnot("Input 'n' must be > 1" = n > 1)
  stopifnot("Standard deviations must be strictly positive" = sdx > 0 & sdy > 0)
  stopifnot("Correlation coefficient must be -1 < corr < 1" = abs(corr) < 1)


  # Generate gaussian random numbers with mean = 0 and sd = 1.
  x <- rnorm(n = n, mean = 0, sd = 1)
  y <- rnorm(n = n, mean = 0, sd = 1)


  # Generate correlated x-y dataset.
  y <- corr * x + sqrt(1 - corr^2) * y


  # Modify means and standard deviations.
  x <- x*sdx + meanx
  y <- y*sdy + meany


  return(data.frame(x = x, y = y))
}
