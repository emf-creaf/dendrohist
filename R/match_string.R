#' Pattern matching
#'
#' @description
#' A match is sought between two character vectors containing regular expressions.
#' If set, search is case-independent, diacritics and tildes are not considered and
#' a reverse is carried out when there is no match.
#'
#' @param pattern a character vector whose elements containing regular expressions to be matched in \code{y}.
#' @param text a character vector where matches are sought. If \code{reverse = T} a further search
#' is performed where the roles of \code{x} and \code{y} are swapped.
#' @param match logical, if set to TRUE matches will be carried out with the \link[base]{match} function;
#' otherwise, \link[base]{regexpr} will be used.
#' @param ignore.case logical, if set to TRUE letter cases are ignored. Default is TRUE.
#' @param remove.accent logical, if set to TRUE diacritics and tilde letters are substituted
#' by the plain versions. Default is FALSE.
#' @param reverse logical, if set to TRUE search is also carried out in reverse mode. Default is FALSE.
#' @param verbose logical, if set to TRUE a progress bar is printed. Default is FALSE.
#'
#' @return
#' A \code{list} object with the same length as \code{pattern}. Each element of the output consists of a
#' \code{data.frame} with two columns: \code{index} and \code{reverse}. The former indicates the index
#' of the first match of the corresponding 'pattern' in vector 'text'. The latter shows whether the match was
#' found in a direct search or a reverse one.
#'
#' @details
#' Simple implementation of the \link[base]{match} or \link[base]{regexpr} functions.
#' Notice that the output will be different in either case. See Examples below.
#'
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
#' print(match_string(x, y, match = F))
#'
match_string <- function(pattern, text, ignore.case = T, remove.accent = T, reverse = F, verbose = F) {


  # Checks.
  stopifnot("Inputs 'pattern' and 'text' must be character vectors" =
              is.vector(pattern) & is.character(pattern) & is.vector(text) & is.character(text))


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
    pattern <- tolower(pattern)
    text <- tolower(text)
  }


  # No diacritics or tilde, please.
  if (remove.accent) {
    pattern <- replace_accent(pattern)
    text <- replace_accent(text)
  }


  # Search each 'pattern' in 'text', and the opposite, if 'reverse' is TRUE.
  ll <- list()
  for (i in 1:length(pattern)) {

    # If verbose = T, show progress bar.
    if (verbose) setTxtProgressBar(pb, i)

    # Empty initial data.frame.
    df <- data.frame(index = numeric(), reverse = logical())

    # Only if pattern[i] is valid.
    if (!is.na(pattern[i])) {

      # First search.
      q <- regexpr(pattern[i], text, ignore.case = ignore.case)
      qNA <- is.na(q)

      # Main loop.
      for (j in 1:length(text)) {

        # First search pattern in text.
        if (!qNA[j]) {
          if (q[j] > 0) {
            df <- rbind(df, data.frame(index = j, reverse = F))
          } else if (reverse) {

            # Unsuccessful. Search text in pattern.
            if (!is.na(text[j])) {
              rq <- regexpr(text[j], pattern[i], ignore.case = ignore.case)
              if (rq > 0) {
                df <- rbind(df, data.frame(index = j, reverse = T))
              }
            }
          }
        }
      }

    }
    ll[[i]] <- df
  }

  if (verbose) cat("\n")

  return(ll)

}
