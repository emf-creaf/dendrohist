#' Read Spanish basin data from the *cedex* site.
#'
#' @description
#' \code{read_cedex_basin} retrieves time series datasets and statistics for
#' several Spanish basins from the Centro de Estudios y Experimentación
#' de Obras Públicas (CEDEX).
#'
#'
#' @param table \code{character} with the name of the file to retrieve from the
#' *cedex* site. If not given the default value is "afliq".
#' @param str_url string with the \code{url} of the *cedex* site. Value by default is
#' "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020/EBRO/". Notice that,
#' internally, \code{read_cedex_basin} will paste \code{table} and \code{str_url} together,
#' so any missing slash will raise an error.
#'
#' @return
#' A \code{data.frame}.
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
read_cedex_basin <- function(table = NULL, str_url = NULL) {


  # Checks.
  if (is.null(table)) {
    table <- "afliq"
  } else {
    stopifnot("Input 'table' must be a single string" = is.character(table) & length(table) == 1)
    table <- tolower(table)


  }


  # URL by default.
  if (is.null(str_url)) {
    str_url <- "https://ceh-flumen64.cedex.es/anuarioaforos//anuario-2019-2020/EBRO/"
  }


  # Get data.
  x <- read.csv2(paste0(str_url, table, ".csv"))


  # Change column names and format.
  if (table ==  "afliq") {
    colnames(x) <- c("Station", "Date", "Height", "Flow")
    x <- set_colmode(x, c("character", "character", "numeric", "numeric"))
    x$Date <- as.Date(x$Date, format = "%d/%m/%Y")
  }


  return(x)



}
