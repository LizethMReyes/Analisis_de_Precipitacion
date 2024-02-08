#Obtener porcentaje de datos por cada una de las estaciones. Utiliza una lista
#Crea una lista para guardar la informacion de datos faltantes
p_missing_values <- list()

#Funcion para iterar en cada uno de los elementos de la lista
for (nombre_estacion in names(lista_estaciones)){
  datos_estacion <- lista_estaciones[[nombre_estacion]]
  datos_faltantes <- sapply(datos_estacion, function(x) sum(is.na(x)))
  p_missing_values[[nombre_estacion]] <- datos_faltantes
}

datos_faltantes <- do.call(rbind, p_missing_values)
#Guarda datos faltantes como un dataframe
datos_faltantes <- as.data.frame(datos_faltantes)
#agrega una columan con el nombre de la estacion 
datos_faltantes$nombre_estacion <- row.names(datos_faltantes)

#Guarda la tabla con una formato CSV
ruta_completa <-"C:/Users/Administrador/Documents/Analisis_de_Precipitacion/Datos/Estaciones/missing_values_est.csv"
write.csv(datos_faltantes, file=ruta_completa, row.names=FALSE)

#Número total para el período de estudio 1095 datos
#crear una copia del dataframe p_missing_values
clean_p_miss_values <- datos_faltantes
clean_p_miss_values <- subset(clean_p_miss_values, selec=-c(Fecha, x, y))

#Obtener procenaje de datos faltantes 
percentage <- clean_p_miss_values
percentage[, 1:4] <- percentage[, 1:4]/1095*100
percentage$Periodo <- "2017-2019-Days"

ruta_completa1 <-"C:/Users/Administrador/Documents/Analisis_de_Precipitacion/Datos/Estaciones/percent_mis_val_est.csv"

write.csv(percentage, file=ruta_completa1, row.names=FALSE)

