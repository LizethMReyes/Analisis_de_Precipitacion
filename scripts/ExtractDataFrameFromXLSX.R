library(readxl)

# Ruta del archivo Excel
archivo_excel <- "~/Downloads/Estacion-Input/Estaciones_huamantlahf-1.xlsx"

# Obtener la lista de nombres de las hojas
hojas <- excel_sheets(archivo_excel)

# Iterar sobre cada hoja
for (hoja in hojas) {
  # Leer la hoja actual
  datos <- read_xlsx(archivo_excel, sheet = hoja, na = "NULL")
  
  # Nombre del archivo CSV
  nombre_csv <- paste0(hoja, ".csv")
  
  # Guardar los datos como CSV
  write.csv(datos, file = nombre_csv, row.names = FALSE)
  
  cat("Hoja '", hoja, "' guardada como '", nombre_csv, "'\n")
}

