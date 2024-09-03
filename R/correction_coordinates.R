#' Correction to some lat-lon coordinates
#'
#' @param df data.frame with a column named "indroea" and two other columns
#' for latitud and longitude.
#'
#' @description
#' A correction is introduced to some lat-long coordinates in some basins
#'
#' @return
#' The input data.frame with corrected coordinates, if needed.
#'
#' @export
#'
#' @details
#' A correction to coordinates, email from Carmen Mirta Dimas Suárez (CEH)
#' on July 17th, 2024. The correction applies to stations 3179, 3191, 3275 and 5107 only.
#'
#' @examples
#' df <- data.frame(indroea = c("3179", "3000"), lat = 1:2, long = 3:4)
#' correction_coordinates(df)
correction_coordinates <- function(df) {


  # Checks.
  stopifnot("Input 'df' must be of data.frame type" = inherits(df, "data.frame"))
  stations <- c("3179", "3191", "3275", "5107")


  # Loop along stations.
  k <- match(df$indroea, stations)
  knotNA <- !is.na(k)
  kstat <- stations[k]
  if (sum(knotNA) > 0) {
    for (i in 1:length(k)) {
      if (knotNA[i]) {
        if (kstat[i] == "3179") {
          df$lat[i] <- 401804
        } else if (kstat[i] == "3191") {
          df$lat[i] <- 404504
        } else if (kstat[i] == "3275") {
          df$lat[i] <- 403003
        } else if (kstat[i] == "5107") {
          df$longwgs84[i] <- -40826
          df$latwgs84[i] <- 380107
        } else {
          message("Wrong station code")
        }
      }
    }
  }


  return(df)


}
