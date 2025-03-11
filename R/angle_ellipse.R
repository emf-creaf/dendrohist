#' Angle of PCA rotation
#'
#' @description
#' \code{angle_ellipse} calculates the angle (counterclockwise from the horizontal X-axis) of the first
#' PCA rotated axis.
#'
#' @param df data.frame with two columns 'x' and 'y' containing the 2D dataset to be analyzed.
#'
#' @returns
#' Angle in degrees.
#'
#' @details
#' The angle is calculated from the PCA loadings.
#'
#' @export
#'
#' @examples
#'
#' # Check the difference between the angle from a regression line and the angle from a PCA analysis.
#' angle1 <- replicate(1000, angle_ellipse(corr2dpoints(1000, corr= .45, sdx = 1, sdy = 6, meanx = 2.3, meany = 5.6)))
#' print(atan(.45*6)*180/pi)
#' angle2 <- replicate(1000, atan(coef(lm(y~x, corr2dpoints(1000, corr= .75, sdx = 1, sdy = 6, meanx = 2.3, meany = 5.6)))[2])*180/pi)
#' x <- c(angle1, angle2)
#' hist(angle1, 20, xlim = c(min(x), max(x)), main = "", xlab = "Angle (deg)", col = "red")
#' hist(angle2, 20, xlim = c(min(x), max(x)), add = TRUE, col = "blue")
#' legend("top", legend = c("Regression", "PCA"), col = c("red", "blue"), pch = 16, cex = 1)
#'
#' # Plot the segments.
#' df <- corr2dpoints(10000, corr= .45, sdx = 1, sdy = 6, meanx = 2.3, meany = 5.6)
#' plot(df, xlab = "X", ylab = "Y", pch = 16, cex = .1)
#' r <- lm(y ~ x, df)
#' xp <- c(-10, 10)
#' points(xp, predict(r, newdata = data.frame(x = xp)), type = "l", lwd = 2, col = "red")
#' y2 <- (xp - 2.3) * tan(mean(angle2)*pi/180) + 5.6
#' points(xp, y2, type = "l", lwd = 2, col = "blue")
#'
angle_ellipse <- function(df) {

  # Checks.
  stopifnot("Input 'df' must be a data.frame with two columns" = is.data.frame(df) & ncol(df) == 2)


  # Principal component analysis.
  pca <- princomp(data.frame(x = df[, 1], y = df[, 2]))


  # Compute the angle (in degrees) counterclockwise from the horizontal X-axis.
  angle <- 90 + atan2(pca$loadings[2,2], pca$loadings[1,2]) * 180 / pi


  return(angle)

}
