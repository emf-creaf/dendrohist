# Nomenclátor Geográfico de Municipios y Entidades de Población from the CNIG database
# (https://centrodedescargas.cnig.es/CentroDescargas/index.jsp and look for **).
muni <- read.table(".\\data-raw\\MUNICIPIOS.csv", sep = ";", dec = ",", header = T, encoding = "latin1",
                quote = "\"", colClasses = c("COD_GEO" = "character"))

# Instituto Nacional de Estadística (INE) database (https://www.ine.es/daco/daco42/codmun/diccionario24.xlsx).
dicc <- readxl::read_excel(".\\data-raw\\diccionario24.xlsx", skip = 1, col_names = T) |>
  as.data.frame()
prov <- readxl::read_excel(".\\data-raw\\Comunidades Autonomas y provincias.xlsx", col_names = T) |>  as.data.frame()




#' Preprocess datasets.
#'
#' @description
#' Little function to:
#' - Substitute curly quotation marks.
#' - Replace diacritics and 'ñ'.
#' - Switch to lower case letters.
#' - Split string by "/" and keep the left part.
#' - Swap words divided by a comma.
#' - Remove white space.
#'
#' @param df \code{data.frame} containing data fro INE or CNIG databases.
#' @param colnam \code{character} string with the name of the column to process.
#'
#' @return
#' The input \code{data.frame} with a modified 'colnam' column.
#'
preprocess <- function(df, colnam) {
  df[, colnam] <- df[, colnam] |> dendrohist::curly_quotes(FALSE) |>
    dendrohist::replace_accent() |>
    tolower() |>
    dendrohist::string_split("/") |>
    dplyr::select(left) |>
    dendrohist::comma_swap() |>
    gsub("' ", "'", .)
  return(df)
}


# Process columns.
muni <- f1(muni, "NOMBRE_ACTUAL")
dicc <- f1(dicc, "NOMBRE")


# Municipal codes.
i <- match(muni$NOMBRE_ACTUAL, dicc$NOMBRE)
muni <- cbind(muni, dicc[i, ] |> dplyr::select(CODAUTO, CPRO, CMUN, DC))
muni$PO <- paste0(dicc$CPRO[i], dicc$CMUN[i])


# Save 'muni'.
save(muni, file = "R/sysdata.rda")




