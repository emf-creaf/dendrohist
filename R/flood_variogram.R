#' Title
#'
#' @param x
#' @param selection
#' @param cutoff
#' @param model_type
#' @param nboot
#'
#' @returns
#' @export
#'
#' @examples
flood_variogram <- function(x, formula = log(num_events) ~ 1, selection = "all", cutoff = 100,
                            width = 1, model_type = "Sph", nboot = 100, verbose = TRUE) {

  # Make sure all strings are lower case.
  selection <- tolower(selection)
  selection <- match.arg(selection, c("all", "ff", "pf", "pr"))

  x$SUB.GROUP.CODE <- tolower(x$SUB.GROUP.CODE)


  # Select subdata.
  x <- switch(selection,
              all = x,
              ff = dplyr::filter(x, SUB.GROUP.CODE == "ff"),
              pf = dplyr::filter(x, SUB.GROUP.CODE == "pf"),
              pr = dplyr::filter(x, SUB.GROUP.CODE == "pr")
  )


  # Extract coordinates from the sf object and calculate the distance matrix using the 'distm' function
  # from the 'geosphere' package.
  coords_matrix <- sf::st_coordinates(x)
  df_events <- as.data.frame(unique(coords_matrix))
  colnames(df_events) <- c("long", "lat")
  dist_events <- geosphere::distm(df_events, fun = geosphere::distGeo)/1000 # In kilometers.


  # Identifiers to indicate locations.
  id <- 1:nrow(df_events)
  x$ID <- id[match(paste(coords_matrix[, 1], coords_matrix[, 2]), paste(df_events[, 1], df_events[, 2]))]
  df_events$num_events <- sapply(id, function(y) sum(x$ID == y))


  df_events <- df_events |> dplyr::filter(num_events < 5)

  # Transform to 'sf' object.
  df_events <- df_events |> sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("EPSG:4326"))


  # Calculate variogram. Response variable is log-transformed.
  df.vgm <- gstat::variogram(formula, data = df_events, width = width)


  # We fit a functional form to the variogram. 'cutoff' makes sure that too distance calculations are not used.
  fit_var <- gstat::fit.variogram(df.vgm[df.vgm$dist < cutoff, ], gstat::vgm(model_type))


  # Store the results.
  out <- list(df = df_events, vgm = df.vgm, fit = fit_var)


  # Do we want bootstrap-based confidence intervals?
  if (nboot > 0) {

    boot_results <- matrix(NA, 2, nboot)

    if (verbose) cli::cli_progress_bar("Computing bootstrap ", total = nboot)

    j <- 1:nrow(df_events)
    for (i in 1:nboot) {

      repeat {

        # Bootstrap resample
        boot_sample <- df_events[sample(j, replace = TRUE), ]

        # Compute the experimental variogram for the bootstrapped sample
        vgram_boot <- gstat::variogram(formula, data = boot_sample, width = width)

        # Fit the variogram model
        fit_boot <- tryCatch(gstat::fit.variogram(vgram_boot, model = gstat::vgm(model_type)),
                             error = function(e) e,
                             warning = function(w) w)
        if (!("error" %in% class(fit_boot)) & !("warning" %in% class(fit_boot))) {
          break
        }
      }

      # Store the model parameters
      boot_results[, i] <- c(sum(fit_boot$psill), fit_boot$range[2])

      if (verbose) cli::cli_progress_update()
    }

    if (verbose) cli::cli_process_done()

    CI <- sapply(1:2, function(k) quantile(boot_results[k, ], c(0.025, 0.5, 0.975)))
    colnames(CI) <- c("sill", "range")
    out$CI <- CI

  }

  return(out)
}
