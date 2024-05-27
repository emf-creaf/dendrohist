#' Pattern matching
#'
#' @description
#' A match is sought between two character vectors containing regular expressions.
#' If set, search is case-independent, diacritics and tildes are not considered and
#' a reverse is carried out when there is no match.
#'
#' @param x a character vector whose elements containing regular expressions to be matched in \code{y}.
#' @param y a character vector where matches are sought. If \code{reverse = T} a further search
#' is performed where the roles of \code{x} and \code{y} are swapped.
#' @param ignore.case logical, if set to TRUE letter cases are ignored. Default is TRUE.
#' @param remove.accent logical, if set to TRUE diacritics and tilde letters are substituted
#' by the plain versions. Default is FALSE.
#' @param reverse logical, if set to TRUE search is also carried out in reverse mode. Default is FALSE.
#' @param verbose logical, if set to TRUE a progress bar is printed. Default is FALSE.
#'
#' @return
#' A \code{list} object with the same length as \code{x}. Each element of the output consists of a
#' \code{data.frame} with three columns: \code{index}
#'
#' @details
#' Simple implementation of the \code{regexpr} function.#'
#'
#' @export
#'
#' @examples
#' x <- c("Buho", "azúcar", "Prague", "Momentum")
#' y <- c("Búho búsqueda de piñatas exóticas con él",
#' "Stop all the clocks",
#' "Azucarillo",
#' "Tráigaselo rápido al marqués con muchísimo azúcar",
#' "le pont d'avignon n'est pas terminé",
#' "Mom")
#' print(match_string(x, y))
#'
match_string <- function(x, y, ignore.case = T, remove.accent = T, reverse = F, verbose = F) {


  # Checks.
  stopifnot("Inputs 'x' and 'y' must be character vectors" =
              is.vector(x) & is.character(x) & is.vector(y) & is.character(y))


  # If progress is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Matching strings...\n"))
    pb <- txtProgressBar(min = 0,
                         max = length(x),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  # Case-neutral search.
  if (ignore.case) {
    x <- tolower(x)
    y <- tolower(y)
  }

  # No diacritics or tilde, please.
  if (remove.accent) {
    x <- replace_accent(x)
    y <- replace_accent(y)
  }


  # Search each 'x' in 'y', and the opposite, if 'reverse' is TRUE.
  l <- list()
  for (i in 1:length(x)) {

    # If verbose = T, show progress bar.
    if (verbose) setTxtProgressBar(pb, i)

    df <- data.frame(index = numeric(), location = numeric(), reverse = logical())

    # Main loop.
    for (j in 1:length(y)) {

      # First search x in y.
      q <- regexpr(x[i], y[j], ignore.case = ignore.case)
      if (any(q > 0)) {
        k <- which(q != -1)
        lk <- length(k)
        df <- rbind(df, data.frame(index = rep(j, lk), location = as.vector(q[k]), reverse = rep(F, lk)))
      } else if (reverse) {

        # Unsuccessful. Search y in x.
        q <- regexpr(y[j], x[i], ignore.case = ignore.case)
        if (any(q > 0)) {
          k <- which(q != -1)
          lk <- length(k)
          df <- rbind(df, data.frame(index = rep(j, lk), location = as.vector(q[k]), reverse = rep(T, lk)))
        }
      }
    }
    l[[i]] <- df
  }

  if (verbose) cat("\n")

  return(l)

}
