#' Check CNIG and INE datasets
#'
#' @description
#' This function checks the consistency of municipal names in the CNIG and INE databases that accompany
#' this package.
#'
#' @param munic_CNIG \code{data.frame} containing the 'MUNICIPIOS' dataset from the
#' *Nomenclator geografico de municipios y entidades de poblacion* at the CNIG web site.
#' @param munic_INE \code{data.frame} containing the 'diccionario24' dataset from the
#' INE web site.
#'
#' @return
#' Informative messages.
#'
#' @details
#' This function is not exported because it is usually used by the package maintainer only.
#'
#' Municipal names have the case lowered, split by slashes into two string, quotation marks
#' modified and commas removed. During the split, the original name is split into 'left' and 'right'
#' versions (which will usually correspond to the Spanish and local language names). Those two versions
#' can be used for checking, although at this moment only 'left' names are used.
#'
#' @examples
#' data(munic_CNIG)
#' data(munic_INE)
#' check_CNIG_INE(munic_CNIG, munic_INE)
check_CNIG_INE <- function(munic_CNIG, munic_INE, verbose = T) {

  # First checks.
  stopifnot("Inputs must be of 'data.frame' class" = is.data.frame(munic_CNIG) & is.data.frame(munic_INE))

x1=munic_CNIG
x2=munic_INE
  # The municipal names in CNIG and INE datasets are preprocessed.
  munic_INE$CODPOSTAL <- paste0(munic_INE$CPRO, munic_INE$CMUN)
  munic_INE$CPRO <- as.numeric(munic_INE$CPRO)


  # Substitute curly quotation marks, if any.
  munic_CNIG$NOMBRE_ACTUAL_nocurly <- curly_quotes(munic_CNIG$NOMBRE_ACTUAL, F)
  munic_INE$NOMBRE_nocurly <- curly_quotes(munic_INE$NOMBRE, F)
  if (verbose) cli::cli_alert_success("Substitution of curly quotation marks by straight ones")



  # Remove diacritics and tildes.
  munic_CNIG$NOMBRE_ACTUAL_nocurly_notilde <- replace_accent(munic_CNIG$NOMBRE_ACTUAL_nocurly)
  munic_INE$NOMBRE_nocurly_notilde <- replace_accent(munic_INE$NOMBRE_nocurly)
  if (verbose) cli::cli_alert_success("Removal diacritics and tildes")


  # Transform to lower case letters.
  munic_CNIG$NOMBRE_ACTUAL_nocurly_notilde_tolower <- tolower(munic_CNIG$NOMBRE_ACTUAL_nocurly_notilde)
  munic_INE$NOMBRE_nocurly_notilde_tolower <- tolower(munic_INE$NOMBRE_nocurly_notilde)
  if (verbose) cli::cli_alert_success("Transform to lower case letters")


  # Split names by slash "/".
  z <- string_split(munic_CNIG$NOMBRE_ACTUAL_nocurly_notilde_tolower, "/")
  munic_CNIG$Nombre_left <- z$left
  munic_CNIG$Nombre_right <- z$right
  z <- string_split(munic_INE$NOMBRE_nocurly_notilde_tolower, "/")
  munic_INE$Nombre_left <- z$left
  munic_INE$Nombre_right <- z$right
  if (verbose) cli::cli_alert_success("Splitting names with slashes")


  # Remove comma and swap words.
  munic_CNIG$Nombre_left <- comma_swap(munic_CNIG$Nombre_left)
  munic_CNIG$Nombre_right <- comma_swap(munic_CNIG$Nombre_right)
  munic_INE$Nombre_left <- comma_swap(munic_INE$Nombre_left)
  munic_INE$Nombre_right <- comma_swap(munic_INE$Nombre_right)
  if (verbose) cli::cli_alert_success("Removal of commas and word-swapping")


  # Silly white space removed when using apostrophe.
  munic_CNIG$Nombre_left <- gsub("' ", "'", munic_CNIG$Nombre_left)
  munic_CNIG$Nombre_right <- gsub("' ", "'", munic_CNIG$Nombre_right)
  munic_INE$Nombre_left <- gsub("' ", "'", munic_INE$Nombre_left)
  munic_INE$Nombre_right <- gsub("' ", "'", munic_INE$Nombre_right)


  # A little extra preprocessing step is added here, whereby "right" names that are
  # identical to their "left" counterparts are eliminated.
  i <- match(munic_CNIG$Nombre_left, munic_CNIG$Nombre_right, incomparables = NA)
  munic_CNIG$Nombre_right[i] <- NA
  i <- match(munic_INE$Nombre_left, munic_INE$Nombre_right, incomparables = NA)
  munic_INE$Nombre_right[i] <- NA

browser()
  # Cross-checking the municipality names between CNIG and INE. For now only the 'left'
  # name will be used.
  for (i in 1:nrow(munic_INE)) {
    j <- munic_INE$Nombre_left[i] %in% munic_CNIG$Nombre_left
    stopifnot("More than one match of INE municipal names in CNIG" = length(j) == 1)
    stopifnot("No match of INE municipal names in CNIG" = !is.na(j))
    j <- munic_CNIG$Nombre_left[i] %in% munic_INE$Nombre_left
    stopifnot("More than one match of CNIG municipal names in INE" = length(j) == 1)
    stopifnot("No match of CNIG municipal names in INE" = !is.na(j))
  }
  if (verbose) cli::cli_alert_success("Cross-check of municipality names")


}
