library(dplyr)
#Leer datos desde la consola de R o importar desde los archivos de este proyecto 
#Separar cada una de las estacines de datos_completos en tablas independientes
lista_estaciones <- split(datos_completos, datos_completos$nombre_estacion)

#Define la carpeta de salida ruta: Analisis_de_Precipitacion|Datos|Estaciones
carpeta_resultados <- file.path("C:/Users/Administrador/Documents/Analisis_de_Precipitacion/Datos/Estaciones")

#Función para guardar una tabla en formato .csv
for (nombre_estacion in names (lista_estaciones)){
  #Obtener datos de cada estación
  datos_estacion <- lista_estaciones[[nombre_estacion]]
  nombre_archivo <- file.path(carpeta_resultados, paste0(nombre_estacion, ".csv"))
  write.csv(datos_estacion, file=nombre_archivo, row.names = FALSE)
}
