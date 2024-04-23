library(readr)

# Obtener la lista de archivos .csv en la carpeta "cleanSatations"
archivos_csv <- list.files(path = "~/Downloads/Estacion-Input/cleanStations/", pattern = "\\.csv", full.names = TRUE)

# Iterar sobre cada archivo .csv
for (archivo in archivos_csv) {
  # Leer el archivo .csv
  dataframe <- read_csv(archivo, col_types = cols(FECHA = col_datetime(format = "%Y-%m-%d")))
  
  # Obtener el año mínimo del dataframe
  primer_anio <- min(as.numeric(format(dataframe$FECHA, "%Y")))
  
  # Crear la fecha inicial y final
  fecha_inicial <- as.Date(paste0(primer_anio, "-01-01"))
  fecha_final <- as.Date("2019-12-31")
  
  # Crear el dataframe con la secuencia completa de fechas
  fechas_completas <- data.frame(FECHA = seq(fecha_inicial, fecha_final, by = "day"))
  
  # Unir el dataframe existente con las fechas completas
  dataframe_completo <- merge(fechas_completas, dataframe, by = "FECHA", all.x = TRUE)
  
  # Cambiar el formato de la columna FECHA
  dataframe_completo$FECHA <- format(dataframe_completo$FECHA, "%d/%m/%y")
  
  # Seleccionar solo las columnas "Fecha" y "Precipitación"
  dataframe_completo <- subset(dataframe_completo, select = c("FECHA", "PRECIP"))
  
  # Renombrar las columnas
  colnames(dataframe_completo) <- c("Fecha", "Precipitacion")
  
  # Crear la carpeta si no existe
  if (!file.exists("secuencia-completa")) {
    dir.create("secuencia-completa")
  }
  
  # Obtener el nombre del archivo de salida sin la ruta ni la extensión
  nombre_salida <- tools::file_path_sans_ext(basename(archivo))
  
  # Ruta del archivo CSV
  ruta_csv <- file.path("secuencia-completa", paste0(nombre_salida, ".csv"))
  
  # Guardar el dataframe en un archivo CSV
  write.csv(dataframe_completo, file = ruta_csv, row.names = FALSE)
  
  # Confirmación
  cat("El archivo CSV se ha guardado en:", ruta_csv, "\n")
}
