#' Extract coordinates from \code{data.frame}
#'
#' @description
#' \code{select_coord} extract x-y coordinates from input \code{data.frame}.
#' Columns are named according to the "efact.csv" file from the CEDEX web site.
#'
#' @param df \code{data.frame} from CEDEX-CEH
#' @param cs \code{character} string indicating which coordinates to select.
#' It can be "utm", "utm30", "wgs84", "ed50" or "etrs89", in upper/lower case letters.
#'
#' @return
#' The input \code{data.frame} "df" with two columns X-Y containing the extracted coordinates.
#'
#' @export
#'
#' @examples
#' # This is not an actual CEDEX-CEH "estaf.csv" gauging station file.
#' df <- data.frame(xutm = round(runif(5)*10000), yutm = round(runif(5)*10000),
#' xutm30 = round(runif(5)*10000), yutm30 = round(runif(5)*10000),
#' xetrs89 = round(runif(5)*10000), yetrs89 = round(runif(5)*10000),
#' long = paste0(paste0("0", sample(2:6, 5)), paste0(sample(20:55, 5), paste0(sample(20:55, 5)))),
#' lat = paste0(paste0(sample(38:44, 5)), paste0(sample(20:55, 5), paste0(sample(20:55, 5)))),
#' longwgs84 = paste0(paste0("0", sample(2:6, 5)), paste0(sample(20:55, 5), paste0(sample(20:55, 5)))),
#' latwgs84 = paste0(paste0(sample(38:44, 5)), paste0(sample(20:55, 5), paste0(sample(20:55, 5)))))
#'
#' # Different outputs depending on the "cs" option.
#' select_coord(df, "utm")
#' select_coord(df, "utm30")
#' select_coord(df, "etrs89")
#' select_coord(df, "ed50")
#' select_coord(df, "wgs84")
#'
select_coord <- function(df, cs) {

  # Usual checks.
  stopifnot("Input 'df' must be a data.frame" = inherits(df, "data.frame"))
  cs <- tolower(cs)
  stopifnot("Input 'cs' must be 'UTM', 'UTM30', 'WGS80', 'ED50' or 'ETRS89'" = any(cs %in% c("utm", "utm30", "wgs84", "ed50", "etrs89")))


  # Choose prefix for CEDEX data.
  m <- switch(cs,
              utm = "utm",
              utm30 = "utm30",
              etrs89 = "etrs89",
              ed50 = "",
              wgs84 = "wgs84")


  # Select coordinates.
  if (any(cs %in% c("utm", "utm30", "etrs89"))) {
    df$x <- as.numeric(df[, paste0("x", m)])
    df$y <- as.numeric(df[, paste0("y", m)])
  } else {
    df$x <- sapply(format(df[, paste0("long", m)], scientific = F, trim = T), function(z) extract_gms(z))
    df$y <- sapply(format(df[, paste0("lat", m)], scientific = F, trim = T), function(z) extract_gms(z))
  }


  return(df)

}
