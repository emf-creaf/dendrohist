#' Remove diacritical marks and tildes (~) from a string.
#'
#' @description
#' A simple function to remove diacritical marks (aka accents) and tildes
#' from a string or a vector of strings.
#' Accentuated and/or letters with tilde are then replaced by their non-accentuated and/or
#' non-tilde version.
#'
#' @param x character vector.
#'
#' @return
#' An non-accentuated, non-tilde version of \code{x}.
#'
#' @details
#' It uses function \code{iconv} from the built-in \code{base} package.
#'
#' @export
#'
#' @examples
#'
#' # In Spanish.
#' x <- c("Búho búsqueda de piñatas exóticas con él",
#' "Tráigaselo rápido al marqués con muchísimo azúcar",
#' "Qué día más tonto")
#' print(rbind(x, replace_accent(x)))
#'
#' # In Catalan.
#' y <- c("Sé que sí, però m'és igual",
#' "Vés-te'n a l'hort a collir pésols",
#' "Àngela, on són les bèsties de la Eulàlia?")
#' print(rbind(y, replace_accent(y)))
#'
#' # In French.
#' z <- c("Où est allé ce très gentil employé à l'élégant béret?",
#' "Ça ne m'étonne pas, j'ai vu ce frêle musicien flâner près des quais, l'air rêveur",
#' "Êtes-vous sûr?")
#' print(rbind(z, replace_accent(z)))
#'
replace_accent <- function(x) {

  stopifnot("Input vector 'x' must be of type 'character'" = is.vector(x) & is.character(x))

  y <- base::iconv(x, to="ASCII//TRANSLIT")

  return(y)

}
