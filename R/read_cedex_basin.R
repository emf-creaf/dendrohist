#' Read Spanish basin data from the *cedex* site.
#'
#' @description
#' \code{read_cedex_basin} retrieves time series datasets and statistics for
#' several Spanish basins from the Centro de Estudios y Experimentación
#' de Obras Públicas (CEDEX).
#'
#'
#' @param table \code{character} with the name of the file to retrieve from the
#' *cedex* site, without extension. If not given, the default value is "afliq".
#' @param str_url string with the \code{url} of the *cedex* site. Value by default is
#' "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020//EBRO//", corresponding to the
#' Ebro basin.
#' Notice that, internally, \code{read_cedex_basin} will paste \code{table} and \code{str_url} together,
#' so any missing slash at the end of \code{str_url} will raise an error.
#' @param cs \code{character} indicating the coordinate system of choice. Values can be
#' "UTM", "UTM30", "WGS84" (default), "ED50" or "ETRS89" in lower or upper case letters (e.g. "wgs84" is valid,
#' as is "Wgs84").
#' @param verbose \code{logical}, if set to TRUE progress messages are printed on screen.
#'
#'
#' @return
#' A spatial \code{sf} object with a EPSG coordinate reference system (unless cs = "utm").
#'
#' @details
#' To see a description of the files to retrieve see
#' "https://ceh.cedex.es/anuarioaforos/demarcaciones.asp" and go to the basin you want
#' to get data for.
#' Regarding coordinates, cs = "UTM" indicates local UTM coordinates, which may be a problem
#' if the sites are located on both sides of the UTM demarcation line, so we do not recommend it.
#' cs = "UTM30" actually means Mercator UTM 30 North with datum WGS84, whereas ETRS89 implies
#' Mercator UTM 30 with datum ETRS89. Finally, cs = "ED50" and "WGS84" are longitude-latitude
#' system with those datums.
#'
#' The \code{encoding} argument of the \code{read.csv2} function is set to "latin1" in order to
#' correctly read diacritics and tildes.
#'
#' @export
#'
#' @examples
#' # Read afliq.csv data
#' x <- read_cedex_basin(cs = "wgs84")
read_cedex_basin <- function(table = "estaf", basin = "all", cs = "wgs84", verbose = T) {


  # Checks.
  if (is.null(table)) {
    table <- "afliq"
  } else {
    stopifnot("Input 'table' must be a single string" = is.character(table) & length(table) == 1)
    table <- tolower(table)
    z <- c("afliq", "mensual_a", "estaf")
    stopifnot("Wrong 'table' value" = any(table %in% z))
  }

  basin <- tolower(basin)
  all_basins <- c("galicia", "cantabrico", "duero", "ebro", "guadalquivir",
                 "guadiana", "jucar", "miño", "mino", "segura", "tajo")
  stopifnot("Wrong basin name(s)" = all(basin %in% c(all_basins, "all")))
  if (any("all" %in% basin)) basin <- all_basins
  basin <- basin[which(basin != "mino")]

  cs <- tolower(cs)
  cs <- match.arg(cs, c("utm", "utm30", "wgs84", "ed50", "etrs89"))


  # URL for datasets.
  str_url <- "https://ceh-flumen64.cedex.es//anuarioaforos//anuario-2019-2020//"


  # Reads data for all basins
  z <- NULL
  for (j in 1:length(basin)) {
    if (verbose) cat(paste0("\n ---> Basin = ", basin[j], " <---\n\n"))

    ba <- switch(basin[j],
                    galicia = "GALICIA%20COSTA",
                    cantabrico = "CANTABRICO",
                    duero = "DUERO",
                    ebro = "EBRO",
                    guadalquivir = "GUADALQUIVIR",
                    guadiana = "GUADIANA",
                    jucar = "JUCAR",
                    miño = "MIÑO-SIL",
                    segura = "SEGURA",
                    tajo = "TAJO")


    # Get data.
    if (verbose) cat(paste0("\n Reading ", table, ".csv data...\n\n"))
    a <- read.csv2(paste0(str_url, ba, "//", table, ".csv"))


    # Change column names and format.
    if (table ==  "afliq") {
      a <- set_colmode(a, c("character", "character", "numeric", "numeric"))
      a$fecha <- as.Date(a$fecha, format = "%d/%m/%Y")

    } else if (table == "mensual_a") {
      a <- set_colmode(a, c(rep("character", 2), rep("numeric", 10)))
      a$fecha <- with(a, as.Date(paste0(substr(anomes, 1, 4), "-", substr(anomes, 5, 6), "-01")))

    } else if (table == "estaf") {
      a <- read.csv2(paste0(str_url, ba, "//estaf.csv"), encoding = "latin1")
      a <- set_colmode(a, c(rep("character", 3), rep("numeric", 19), rep("character", 5), rep("numeric", 2), "character"))

    }


    # Input is not "estaf".
    if (table != "estaf") {
      b <- read.csv2(paste0(str_url, "estaf.csv"), encoding = "latin1")
      b <- set_colmode(b, c(rep("character", 3), rep("numeric", 19), rep("character", 5), rep("numeric", 2), "character"))

    } else b <- a


    # Coordinates.
    b <- select_coord(b, cs)
    i <- match(a$indroea, b$indroea)
    a$x <- b$x[i]
    a$y <- b$y[i]


  # sf object.
    a <- a |> sf::st_as_sf(coords = c("x", "y"))
    if (cs != "utm") {
      sf::st_crs(a) <- switch(cs,
                              utm30 = 32630,
                              etrs89 = 25830,
                              wgs84 = 4326,
                              ed50 = 4230)
    }
    a$cuenca <- basin[j]


    # Add rows.
    z <- rbind(z, a)

  }


  # Remove trailing/leading blanks from the "lugar" field.
  z$lugar <- trimws(z$lugar)


  return(z)
}
