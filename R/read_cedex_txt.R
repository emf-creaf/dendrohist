#' Read cedex time series data
#'
#' @description
#' \code{read_cedex_txt} reads time series data from the cedex site.
#'
#'
#' @param x url corresponding to the text file to be fetched.
#' @param type character to specify whether the file contains 'daily' or 'monthly' data.
#'
#' @return
#' A \code{data.frame} containing a time series. If \code{type="daily"} the \code{data.frame}
#' will have four columns named "Station", "Date", "Height", and "Flow".
#' if \code{type="monthly"} the \code{data.frame} will have three columns
#' named "Station", "Date" and "Flow".
#'
#' @details
#' Datasets are read and their headers checked.
#'
#' @export
#'
#' @examples
#' x <- "https://ceh.cedex.es/anuarioaforos/temp/resultado8187.txt"
#' df <- read_cedex(x, type = "daily", station_label = "9020")

read_cedex_txt <- function(x, type = NULL, station_label = NULL) {


  # Checks.
  stopifnot("'type' must be set to 'daily' or 'monthly'" = !is.null(type))
  type <- tolower(type)
  stopifnot("Value of 'type' must be 'daily' or 'monthly'" = type %in% c("daily", "monthly"))
  if (!is.null(station_label)) {
    stopifnot("Input 'station_label' should be a string" = is.character(station_label))
  }


  # Little functions to be used below.
  fr <- function(z, ns) unname(unlist(read.table(url(z), skip = ns, nrow = 1)))
  fd <- function(z) unique(strsplit(z,"")[[1]]) == "-"


  # Checking the content of the files.
  if (type == "daily") {


    # Label of first column in row 5 must be "Estac.".
    stopifnot("First column in daily data file must have label 'Estac.'" = fr(x, 4)[1] == "Estac.")


    # Row 5 contains a straight line made up of dashes.
    stopifnot("Fifth row in daily data file should contain only dashes '-'" = fd(fr(x, 5)))


    # Length of 6th row must also be 4. Optionally, check out the label in the first column.
    y <- fr(x, 6)
    stopifnot("There must be 4 columns in daily data file" = length(y) == 4)
    if (!is.null(station_label)) {
      stopifnot("Sixth line in data file does not start with name of the station" = y[1] == station_label)
    }


  } else {


    # Names of months in Spanish.
    data("mes_abr", package = "dendrohist")


    # The third row in the file must contain the names of the months, starting at October.
    # Months are in Spanish.
    stopifnot("Columns in monthly data file are not correct" =
                identical(fr(x, 3)[-c(1:2)], c(substring(roll(mes_abr, 3), 1, 3), "Total")))


    # Line 5 contains a straight line made up of dashes.
    stopifnot("Fifth row in monthly data file should contain only dashes '-'" = fd(fr(x, 5)))


    # Length of 6th row must be 16. Optionally, check out the label in the first column.
    y <- fr(x, 6)
    stopifnot("There must be 16 columns in monthly data file" = length(y) == 16)
    if (!is.null(station_label)) {
      stopifnot("Sixth line in data file does not start with name of the station" = y[1] == station_label)
    }

  }


  # If checks are successful, we proceed to read the file.
  if (type == "daily") {

    # Fetching all daily data for station x.
    df <- read.table(url(x), skip = 6)


    # Rename columns.
    colnames(df) <- c("Station", "Date", "Height", "Flow")


    # Change column format.
    df <- set_colmode(df, c("character", "character", "double", "double"))
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")


    # Set -100 to NA.
    df$Height[df$Height == -100] <- NA
    df$Flow[df$Flow == -100] <- NA


  } else {


    # Names of months in Spanish.
    mon <- roll(month.name, 3)


    # Fetching monthly data for station x.
    y <- read.table(url(x), skip = 6)


    # Rename columns.
    colnames(y) <- c("Station", "Type", "Years", mon, "Total")


    # Extract first year of the pair.
    y$year1 <- substring(y$Years, 1, 4)


    # Calculate second year.
    year <- ifelse(y$year1 < 1999, "19", "20")
    y$year2 <- paste0(year, substring(y$Years, 6, 7))


    # Convert data.frame from wide to long.
    df <- data.frame("Station" = character(),
                     "Month" = character(),
                     "Year" =  character(),
                     "Flow" = numeric())

    # From wide to long.
    for (i in 1:nrow(y)) {

      # October, November and December, and then January, February, etc., but for next year.
      z <- unlist(y[i, ])
      df <- rbind(df, data.frame("Station" = rep(z[1], 12),
                                 "Month" = c(mon[1:3], mon[-c(1:3)]),
                                 "Year" = c(rep(z["year1"], 3), rep(z["year2"], 9)),
                                 "Flow" = z[4:15]),
                  make.row.names = F)
    }


    # Change months to numbers and add a "Date" column in Date format.
    df$Month <- month_to_number(df$Month)
    df$Date <- with(df, as.Date(paste0("01-", Month, "-", Year), "%d-%m-%Y"))
    df <- df[, c("Station", "Date", "Flow")]
    df <- set_colmode(df, c("character", "character", "double"))
    df$Date <- as.Date(df$Date, format = "%Y-%m-%d")


    # Set -100 to NA.
    df$Flow[df$Flow == -100] <- NA
  }

  return(df)
}
