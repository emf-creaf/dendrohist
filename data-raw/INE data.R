## Obtención de datos públicos del Instituto Nacional de Estadística.


# Relación de municipios y códigos por comunidades autónomas y provincias a 1 de enero de 2024
x <- "https://www.ine.es/daco/daco42/codmun/diccionario24.xlsx"

munic_code <- openxlsx::read.xlsx(xlsxFile = x, sheet = 1, startRow = 2)

# Save in data directory.
usethis::use_data(munic_code, overwrite = TRUE)



# Documentation.
usethis::use_r("munic_code")

