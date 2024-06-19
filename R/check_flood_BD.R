#' Check internal and external consistency of the Spanish historical catastrofic flood database.
#'
#' @param flood
#' @param test_CNIG_INE if set to TRUE the CNIG and INE databases are checked for consistency.
#' It is left as an option for future developments, but users should use the default value of FALSE.
#'
#' @return
#'
#'
#' @details
#'
#' For more information see \link[dendrohist]{check_CNIG_INE}.
#'
#'
#' @export
#'
#' @examples
check_flood_BD <- function(flood) {


  # Load CNIG and INE data.
  data(munic_CNIG)
  data(munic_INE)


  # Add municipal code to CNIG database. At this moment only the 'left' names are
  # used.
  munic_CNIG$CODPOSTAL <- ""
  for (i in 1:nrow(munic_INE)) {
    j <- munic_INE$Nombre_left[i] %in% munic_CNIG$Nombre_left
    munic_CNIG$CODPOSTAL[j] <- munic_INE$CODPOSTAL[i]
  }


  ############### Preprocessing of the flood database.
  flood$MUNICIPALITY_nocurly <- curly_quotes(flood$MUNICIPALITY, F)

  flood$MUNICIPALITY_nocurly_notilde <- replace_accent(flood$MUNICIPALITY_nocurly)

  flood$MUNICIPALITY_nocurly_notilde_tolower <- tolower(flood$MUNICIPALITY_nocurly_notilde)

  z <- string_split(flood$MUNICIPALITY_nocurly_notilde_tolower, "/")
  flood$Nombre_left <- z$left
  flood$Nombre_right <- z$right

  flood$Nombre_left <- dendrohist::comma_swap(flood$Nombre_left)
  flood$Nombre_right <- dendrohist::comma_swap(flood$Nombre_right)

  flood$Nombre_left <- gsub("' ", "'", flood$Nombre_left)
  flood$Nombre_right <- gsub("' ", "'", flood$Nombre_right)

  i <- match(flood$Nombre_left, flood$Nombre_right, incomparables = NA)
  flood$Nombre_right[i] <- NA

  flood$CODPOSTAL <- gsub("\\\"", "", flood$`MUNICIPAL CODE (IGN+INSEE)`)


  # If a municipal name AND municipal code are missing for a location, stop with message.


}
