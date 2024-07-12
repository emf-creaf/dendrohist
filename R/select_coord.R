#' Title
#'
#' @param df
#' @param cs
#'
#' @return
#' @export
#'
#' @examples
select_coord <- function(df, cs) {

  # Usual checks.
  stopifnot("Input 'df' must be a data.frame" = inherits(df, "data.frame"))
  cs <- tolower(cs)
  stopifnot("Input 'cs' must be 'UTM', 'UTM30', 'WGS80', 'ED50' or 'ETRS89'" = any(cs %in% c("utm", "utm30", "wgs84", "ed50", "etrs89")))


  # Choose reference system for CEDEX data.
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
    df$x <- sapply(as.character(df[, paste0("long", m)]), function(z) extract_gms(as.character(z)))
    df$y <- sapply(as.character(df[, paste0("lat", m)]), function(z) extract_gms(as.character(z)))
  }


  return(df)

}
