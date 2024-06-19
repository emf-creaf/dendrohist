#' Check internal and external consistency of the Spanish historical catastrofic flood database.
#'
#' @param flood
#' @param test_CNIG_INE if set to TRUE the CNIG and INE databases are checked for consistency.
#' It is left as an option for future developments, but users should use the default value of FALSE.
#'
#' @return
#' Nothing. Messages are printed on screen informing of the progress.
#'
#' @details
#'
#' For more information see \link[dendrohist]{check_CNIG_INE}.
#'
#'
#' @export
#'
#' @examples
check_flood_BD <- function(flood, verbose = T) {


  # First check.
  stopifnot("Input 'flood' must be of 'data.frame' class" = is.data.frame(flood))


  # Load CNIG and INE data.
  data(munic_CNIG)
  data(munic_INE)


  # Add postal codes to both databases.
  if (verbose) cat("\n\n Adding postal codes to both databases...")
  munic_INE$CODPOSTAL <- paste0(munic_INE$CPRO, munic_INE$CMUN)
  munic_INE$CPRO <- as.numeric(munic_INE$CPRO)
  j <- match(munic_INE$Nombre_left, munic_CNIG$Nombre_left)
  munic_CNIG$CODPOSTAL <- munic_INE$CODPOSTAL[j]
  if (verbose) cat(" Done!\n")


  ############### Preprocessing of the flood database. We use only the 'left' name for
  # the time being.
  if (verbose) cat("\n\n Substituting curly quotation marks by straight ones...")
  flood$MUNICIPALITY_nocurly <- curly_quotes(flood$MUNICIPALITY, F)
  if (verbose) cat(" Done!\n")

  if (verbose) cat("\n Removing diacritics and tildes...")
  flood$MUNICIPALITY_nocurly_notilde <- replace_accent(flood$MUNICIPALITY_nocurly)
  if (verbose) cat(" Done!\n")

  flood$MUNICIPALITY_nocurly_notilde_tolower <- tolower(flood$MUNICIPALITY_nocurly_notilde)

  if (verbose) cat("\n Splitting names with slashes '/'...")
  z <- string_split(flood$MUNICIPALITY_nocurly_notilde_tolower, "/")
  flood$Nombre_left <- z$left
  flood$Nombre_right <- z$right
  if (verbose) cat(" Done!\n")

  if (verbose) cat("\n Removing commas and swapping words...")
  flood$Nombre_left <- comma_swap(flood$Nombre_left)
  flood$Nombre_right <- comma_swap(flood$Nombre_right)
  if (verbose) cat(" Done!\n")

  # Remove blanks.
  flood$Nombre_left <- gsub("' ", "'", flood$Nombre_left)
  flood$Nombre_right <- gsub("' ", "'", flood$Nombre_right)

  # If left and right names are the same, set right one to NA.
  i <- match(flood$Nombre_left, flood$Nombre_right, incomparables = NA)
  flood$Nombre_right[i] <- NA

  # New field.
  flood$CODPOSTAL <- gsub("\\\"", "", flood$`MUNICIPAL CODE (IGN+INSEE)`)


  # If a municipal name AND municipal code are missing for a location, stop with message.


  # Matches.
  x <- paste0(bd$Nombre_left, "-", bd$NEW_MUN_CODE)
  y <- paste0(a$Nombre_left, "-", a$MUNCODE)
  i <- match(x, y)

}
