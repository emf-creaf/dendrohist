# Nomenclátor Geográfico de Municipios y Entidades de Población from the CNIG database
# (https://centrodedescargas.cnig.es/CentroDescargas/index.jsp and look for **).
muni <- read.table(".\\data-raw\\MUNICIPIOS.csv", sep = ";", dec = ",", header = T, encoding = "latin1",
                quote = "\"", colClasses = c("COD_GEO" = "character"))

# Instituto Nacional de Estadística (INE) database (https://www.ine.es/daco/daco42/codmun/diccionario24.xlsx).
dicc <- readxl::read_excel(".\\data-raw\\diccionario24.xlsx", skip = 1, col_names = T)

# Substitute curly quotation marks, if any.
muni$NOMBRE_ACTUAL_nocurly <- dendrohist::curly_quotes(muni$NOMBRE_ACTUAL, F)
dicc$NOMBRE_nocurly <- dendrohist::curly_quotes(dicc$NOMBRE, F)
