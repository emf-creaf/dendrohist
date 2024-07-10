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
#' "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020/EBRO/", corresponding to the
#' Ebro basin.
#' Notice that, internally, \code{read_cedex_basin} will paste \code{table} and \code{str_url} together,
#' so any missing slash at the end of \code{str_url} will raise an error.
#' @param English logical, if set to TRUE the names of the columns of the output \code{data.frame} are
#' translated into English. If set to FALSE (default), Spanish names (as in the original files) are kept.
#'
#' @return
#' A \code{data.frame}. Column labels vary depending on the value of "table":
#'
#' * \code{table = "afliq"}: in Spanish, "indroea", "fecha", "altura" and "caudal"; in English, "Station", "Date", "Height" and "Flow".
#' * \code{table = "mensual_a"}: in Spanish, "indroea", "anomes", "hmedmes", "qmedmes", "apormedmes", "qcmes", #' "hcmes", "dia_cmes", "qnmes", "hnmes", "dia_nmes", "orig_dato_id";
#' in English, "Station", "Date", "Height", "Flow", "Streamflow", "Flow_max", "Height_max", "Flow_max_day", "Flow_min", "Height_min", "Flow_min_day", "Orig_data").
#'
#' @details
#' To see a description of the files to retrieve see
#' "https://ceh.cedex.es/anuarioaforos/demarcaciones.asp" and go to the basin you want
#' to get data for.
#'
#' @export
#'
#' @examples
#' x <- read_cedex_basin()
read_cedex_basin <- function(table = NULL, str_url = NULL, English = F) {


  # Checks.
  if (is.null(table)) {
    table <- "afliq"
  } else {
    stopifnot("Input 'table' must be a single string" = is.character(table) & length(table) == 1)
    table <- tolower(table)
    z <- c("afliq", "mensual_a")
    stopifnot("Wrong 'table' value" = any(table %in% z))
  }


  # URL by default, corresponding to the Ebro basin.
  if (is.null(str_url)) {
    str_url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020/EBRO/"
  }


  # Get data.
  x <- read.csv2(paste0(str_url, table, ".csv"))


  # Change column names and format.
  if (table ==  "afliq") {
    x <- set_colmode(x, c("character", "character", "numeric", "numeric"))
    x$fecha <- as.Date(x$fecha, format = "%d/%m/%Y")
    if (English) colnames(x) <- c("Station", "Date", "Height", "Flow")
  } else if (table == "mensual_a") {
    if (English) colnames(x) <- c("Station", "anomes", "Height", "Flow", "Streamflow", "Flow_max",
                                  "Height_max", "Flow_max_day", "Flow_min", "Height_min", "Flow_min_day", "Orig_data")
    x <- set_colmode(x, c(rep("character", 2), rep("numeric", 10)))
    x$Date <- with(x, as.Date(paste0(substr(anomes, 1, 4), "-", substr(anomes, 5, 6), "-01")))
    x$anomes <- NULL
    x <- x[, c("Station", "Date", colnames(x)[-c(1, 12)])]
  }


  return(x)
}
