#' Generate a sample of correlated X-Y gaussian points.
#'
#' @description
#'
#' @param n integer, number of points
#' @param mean
#' @param sd
#' @param corr
#' @param angle angle in degrees, clockwise.
#'
#' @returns
#' @export
#'
#' @examples
#' plot(corr2dpoints(1000, corr = .29, angle = 50))
corr2dpoints <- function(n = 100, mean = 0, sd = 1, corr = 0, angle = 0) {

  # Checks.
  stopifnot("Input 'n' must be > 1" = n > 1)


  if (length(mean) == 1) mean <- c(mean, mean)
  if (length(sd) == 1) sd <- c(sd, sd)


  # First
  x <- rnorm(n = n, mean = 0, sd = 1)
  y <- rnorm(n = n, mean = 0, sd = 1)


  # Correlated 2D points.
  y <- corr * x + sqrt(1 - corr^2) * y


  # Rotation.
  angle <- (angle+45) * pi / 180
  ca <- cos(angle)
  sa <- sin(angle)
  rotmat <- matrix(c(ca, -sa, sa, ca), 2, 2)
  df <- t(rotmat %*% rbind(x, y))


  # Correct for means and standard deviations.
  df[, 1] <- df[, 1] * sd[1] + mean[1]
  df[, 2] <- df[, 2] * sd[2] + mean[2]


  return(data.frame(x = df[, 1], y = df[, 2]))
}
