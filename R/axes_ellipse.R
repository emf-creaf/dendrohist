#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
#' x <- rnorm(1000)
#' y <- rnorm(1000, sd = 10)
axes_ellipse <- function(df, center = FALSE) {

  # Checks.
  stopifnot("Input 'df' must be a data.frame with two columns" = is.data.frame(df) & ncol(df) == 2)

plot(df, pch = 16, cex = .1)
  pca <- princomp(data.frame(x = df[, 1], y = df[, 2]))


  # pca1 <- pca$rotation[, 1]
  # pca2 <- pca$rotation[, 2]
  #
  #
  angle <- 90 + atan2(pca$loadings[2,2], pca$loadings[1,2])*180/pi
  #
  # # Original axes.
  # original_axis1 <- c(1, 0)
  # original_axis2 <- c(0, 1)
  #
  # cosinus1 <- sum(original_axis1 * pca1) / (sqrt(sum(original_axis1^2)) * sqrt(sum(pca1^2)))
  # cosinus2 <- sum(original_axis2 * pca2) / (sqrt(sum(original_axis2^2)) * sqrt(sum(pca2^2)))
  # angle1 <- acos(cosinus1)
  # angle2 <- acos(cosinus2)


  return(angle)

}
