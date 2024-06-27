#' Convert *cedex* daily data to monthly
#'
#' @description
#' \code{daily_to_monthly} calculates monthly averages and totals of the "Height" and
#' "Flow" columns, respectively.
#'
#'
#' @param x \code{data.frame}
#' @param table \code{character} with the name of the type of table, which in turn will
#' determine the columns in \code{x}. Default value is "afliq".
#'
#' @return
#' A \code{data.frame} with the monthly averages and totals of the "Height" and
#' "Flow" columns, respectively.
#'
#' @details
#' Total monthly flow for February is calculated accounting for a leap year, if required.
#' NA's are eliminated and monthly total values are then computed by projecting to the
#' number of days per month.
#'
#' @export
#'
#' @examples.
#' # All stations at once.
#' x1 <- read_cedex_basin()
#' y1 <- daily_to_monthly(x1)
#'
#' # Only one station.
#' x2 <- read_cedex_txt("https://ceh.cedex.es/anuarioaforos/temp/resultado8187.txt", "daily")
#' y2 <- daily_to_monthly(x2)
#'
#' # They match.
#' i <- match(paste0(y2$Station, y2$Flow), paste0(y1$Station, y1$Flow))
#' print(sum(abs(y2$Flow - y1$Flow[i]), na.rm = T))
#'
daily_to_monthly <- function(x, table = NULL) {


  # Checks.
  stopifnot("Input 'x' must be a 'data.frame'" = is.data.frame(x))
  if (is.null(table)) table <- "afliq"
  if (table == "afliq") {
    stopifnot("Wrong columns in 'x'" = all(c("Station", "Date", "Height", "Flow") %in% colnames(x)))
  }


  # Calculations.
  if (table == "afliq") {
    df <- x |>
      dplyr::mutate(month = as.numeric(format(Date, "%m")),
                    year = as.numeric(format(Date, "%Y"))) |>
      dplyr::group_by(Station, year, month) |>
      dplyr::mutate(days = ifelse(is_leap_year(Date),
                                  days_per_month2[month], days_per_month[month])) |>
      dplyr::summarize(Flow = sum(Flow, na.rm = T) / sum(!is.na(Flow)) * max(days),
                       Height = mean(Height, na.rm = T), .groups = "keep") |>
      dplyr::ungroup() |>
      dplyr::mutate(Date = as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")) |>
      dplyr::select(Station, Date, Height, Flow)
  }


  return(df)

}
