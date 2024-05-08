#' Imputation of NA in time series
#'
#' @description
#' It inputs missing values (labelled as NA) in a time series with several
#' methodologies.
#'
#' @param x a \code{numeric} vector or \code{ts} object containing the time series.
#' Internally it is coerced into a vector.
#' @param methods \code{character} vector containing the names of the imputation methods to
#' be applied.
#' @param verbose \code{logical}, if set to TRUE messages are printed on the screen.
#' @param ... Other input parameters for some of the imputation functions.
#'
#' @return
#' A vector with the reconstructed time series.
#'
#' @export
#'
#' @examples
ts_imputations <- function(x, methods = NULL, verbose = T, ...) {

  # Input must be a vector.
  stopifnot("Input 'x' must be a vector" = is.vector(x))


  # Check methods.
  meth <- c("mean", "median", "mode", "interpolation", "locf", "seadec_mean", "ma", "kalman", "ssa")
  if (!is.null(methods)) {
    stopifnot("Input 'methods' must be of type 'character'" = is.character(methods))
    stopifnot("Input 'methods' value is not valid" = any(methods %in% meth))
  } else {
    methods <- meth
  }


  # Other arguments.
  arguments <- list(...)
  if (any("ssa" %in% methods)) {
    groups <- ifelse(length(arguments[["groups"]]) == 0, 1:5, arguments$groups)
    L <- ifelse(length(arguments[["L"]]) == 0, round(length(x)/2), arguments$L)
  }
  if (any("ma" %in% methods)) {
    k <- ifelse(length(arguments[["k"]]) == 0, 4, arguments$L)
    weighting <- ifelse(length(arguments[["weighting"]]) == 0, "exponential", arguments$weighting)
  }


  # Missing Value Imputation by several methods.
  r <- list()
  for (i in methods) {
    if (verbose) cat(paste0("\n -- > ts_imputations: ", i, " method...\n\n"))
    r[[i]] <- switch(i,
                     mean = imputeTS::na_mean(x, option = "mean"),
                     median = imputeTS::na_mean(x, option = "median"),
                     mode = imputeTS::na_mean(x, option = "mode"),
                     interpolation = imputeTS::na_seadec(x, algorithm = "interpolation"),
                     locf = imputeTS::na_seadec(x, algorithm = "locf"),
                     seadec_mean = imputeTS::na_seadec(x, algorithm = "mean"),
                     ma = imputeTS::na_seadec(x, algorithm = "ma", k = k, weighting = weighting),
                     kalman = imputeTS::na_kalman(x, model = "StructTS", smooth = TRUE, nit = -1, maxgap = Inf),
                     ssa = Rssa::gapfill(Rssa::ssa(x, force.decompose = FALSE, L = L), drop = T, groups = groups)
    )
  }


  return(r)

}
