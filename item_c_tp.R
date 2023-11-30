
# c) Calcular la cantidad de meses que se ubicaron por encima de dos veces el desvio
# estandar de cada estacion. Mostrar en una tabla el mes y el anio y el acumulado de
# precipitacion de ese mes.

## Hago los calculos para cada archivo, primero con OCBA y luego con CORR 
#### ------------------- OCBA ----------------------------
# limpio el environment
rm(list = ls())

# defino la ruta del archivo con los datos de ocba
archivo_ocba <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_OCBA_w.csv" 

# abro el archivo con la funcion read.table
datos_ocba = read.table(archivo_ocba, header = F, sep = ",")  
# le asigno nombres a las colummnas
colnames(datos_ocba) <- c("cod. de id", "anio", "mes", "pp") 
head(datos_ocba) # veo las primeras 6 filas 
tail(datos_ocba) # veo las ultimas 6 filas


# defino las variables para los meses y los anios del periodo para esta estacion
meses <- c(1:12) 
anios_ocba <- c(1908:2007) 

## asigno NA a los datos faltantes para luego poder calcular la media y el desvio
datos_ocba$pp[which(datos_ocba$pp==-999.9)] <- NA 
head(datos_ocba) # observo que se hayan asignado correctamente

# luego debo quedarme con los datos de cada mes por separado, me creo una lista donde cada contenedor sea un mes
datos_xmes_ocba <- list() 
for (i in meses) { 
  datos_xmes_ocba[[i]] <- datos_ocba$pp[which(datos_ocba$mes==i)] 
} 
datos_xmes_ocba 

# por medio de lapply puedo calcular la media y desvio estandar para cada contenedor de la lista,  
# que cada contenedor representa un mes, lapply devuelve listas y con na.rm = T no tengo en cuenta los datos NA
medias_ocba = lapply(datos_xmes_ocba, function(x) mean(x, na.rm = T)) 
medias_ocba 

desvios_ocba = lapply(datos_xmes_ocba, function(x) sd(x, na.rm = T)) 
desvios_ocba 

# me genero vectores con los datos del desvio y la media, ya que ggplot acepta data frames 
# y necesito vectores para formarlos 
vector_medias_ocba = c() 
vector_desvios_ocba = c() 
for (i in meses) { 
  vector_medias_ocba = c(vector_medias_ocba, medias_ocba[[i]])  
  vector_desvios_ocba = c(vector_desvios_ocba, desvios_ocba[[i]]) 
} 

# genero un vector con 2 veces el desvio para cada mes  
dos_desv_ocba = c() 
for (i in meses) { 
  dos_desv_ocba[i] = vector_desvios_ocba[i]*2 
} 
dos_desv_ocba 

# creo una lista con los anios correspondiente a cada mes 
aniosde_cadames_ocba <- list() 
for (i in meses) { 
  aniosde_cadames_ocba[[i]] <- datos_ocba$anio[which(datos_ocba$mes==i)] 
} 
aniosde_cadames_ocba # cada contenedor me muestra los anios en los cuales hubo dato de ese mes

# genero un vector donde se almacene la cantidad de veces que un mes se ubico por encima de dos veces el desvio 
cant_veces_xmes_ocba_new = c() 
cant_veces_xmes = 0
# defino una lista donde cada contenedor es un vector con todas las posiciones de los meses
posiciones_xencima = list() 
for (i in meses) { 
  posiciones = c()
  for (j in 1:length(datos_xmes_ocba[[i]])) {
    if (is.na(datos_xmes_ocba[[i]][j])) { 
      next 
    } else if ((datos_xmes_ocba[[i]][j]) > (dos_desv_ocba[i]))    {
      cant_veces_xmes = cant_veces_xmes + 1 
      cant_veces_xmes_ocba_new[i] = cant_veces_xmes 
      posiciones = c(posiciones, j)
      posiciones_xencima[[i]] = posiciones
    } 
  }
  cant_veces_xmes = 0 # redefino el valor de esta variable para el siguiente valor de i  
} 
cant_veces_xmes_ocba_new # me muestra cuantas veces cada mes estuvo por encima de dos veces el desvio
posiciones_xencima # cada contenedor me muestra las posiciones de los meses donde estuvieron por encima

# para armar la tabla debo buscar los anios en que los meses estuvieron por encima de dos veces el desvio 
# mientras que el acumulado ya es el valor que tengo por dato, pues datos mensuales 

## con estas tres listas y la de posiciones, me armo la tabla pedida
datos_xmes_ocba
aniosde_cadames_ocba
cant_veces_xmes_ocba_new
posiciones_xencima

# genero la tabla final con todos los meses, anios y valores acumulado por encima de dos veces el desvio
tabla_final_ocba = data.frame()
for (i in meses) {
  df_mes = data.frame("Mes"=meses[i], "Anio"=aniosde_cadames_ocba[[i]][posiciones_xencima[[i]]], "Acumulado"=datos_xmes_ocba[[i]][posiciones_xencima[[i]]])
  tabla_final_ocba = rbind(tabla_final_ocba, df_mes)
}
tabla_final_ocba

# datos pedidos por la consigna para la estacion OCBA
cant_veces_xmes_ocba_new
tabla_final_ocba

##### ------------------- CORR -------------------------

# defino la ruta para el archivo con los datos de Corrientes
archivo_corr <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_CORR_w.csv"
# abro el archivo con la funcion read.table
datos_corr <- read.table(archivo_corr, header = F, sep = ",")
# le asigno nombres a las columnas de la tabla
colnames(datos_corr) <- c("cod. de id", "anio", "mes", "pp")
head(datos_corr)
tail(datos_corr)

# defino una variable con el periodo de anios para esta estacion
anios_corr <- c(1961:2007)
anios_corr

# le asigno NA a los datos faltantes
datos_corr$pp[which(datos_corr$pp==-999.9)] <- NA
datos_corr

# luego debo quedarme con los datos de cada mes por separado, me creo una lista donde cada contenedor sea un mes 
datos_xmes_corr <- list()
for (i in meses) {
  datos_xmes_corr[[i]] <- datos_corr$pp[which(datos_corr$mes==i)]
}
datos_xmes_corr

# por medio de lapply puedo calcular la media y desvio estandar para cada contenedor de la lista, 
# sin tener en cuenta los NA, lapply devuelve listas
medias_corr = lapply(datos_xmes_corr, function(x) mean(x, na.rm = T))
medias_corr

desvios_corr = lapply(datos_xmes_corr, function(x) sd(x, na.rm = T))
desvios_corr

# me creo vectores con los datos de la media y desvio calculados
vector_medias_corr = c()
vector_desvios_corr = c()
for (i in meses) {
  vector_medias_corr = c(vector_medias_corr, medias_corr[[i]]) 
  vector_desvios_corr = c(vector_desvios_corr, desvios_corr[[i]])
}

# genero un vector con 2 veces el desvio para cada mes  
dos_desv_corr = c() 
for (i in meses) { 
  dos_desv_corr[i] = vector_desvios_corr[i]*2 
} 
dos_desv_corr

# creo una lista con los anios correspondiente a cada mes 
aniosde_cadames_corr <- list() 
for (i in meses) { 
  aniosde_cadames_corr[[i]] <- datos_corr$anio[which(datos_corr$mes==i)] 
} 
aniosde_cadames_corr 

# genero un vector donde se almacene la cantidad de veces que un mes se ubico por encima de dos veces el desvio 
cant_veces_xmes_corr = c() 
cant_veces_xmes = 0
# defino una lista deonde cada contenedor es un vector con todas las posiciones de los meses
posiciones_xencima_corr = list() 
for (i in meses) { 
  posiciones = c()
  for (j in 1:length(datos_xmes_corr[[i]])) {
    if (is.na(datos_xmes_corr[[i]][j])) { 
      next 
    } else if ((datos_xmes_corr[[i]][j]) > (dos_desv_corr[i]))    {
      cant_veces_xmes = cant_veces_xmes + 1 
      cant_veces_xmes_corr[i] = cant_veces_xmes 
      posiciones = c(posiciones, j)
      posiciones_xencima_corr[[i]] = posiciones
    } 
  }
  cant_veces_xmes = 0 # redefino el valor de esta variable para el siguiente valor de i  
} 
cant_veces_xmes_corr # muestra la cantidad de veces que cada mes estuvo por encima de dos veces el desvio
posiciones_xencima_corr # representa las posiciones de cada mes que estan por encima de dos veces el desvio

# para armar la tabla debo buscar los anios en que los meses estuvieron por encima de dos veces el desvio 
# mientras que el acumulado ya es el valor que tengo por dato, pues datos mensuales 

# con estas tres listas y la de arriba de posiciones, me armo la tabla pedida
datos_xmes_corr
aniosde_cadames_corr
cant_veces_xmes_corr # sirve para corroborar el length del df respecto al mes

# genero la tabla final con todos los meses, anios y valores acumulado por encima de dos veces el desvio
tabla_final_corr = data.frame()
for (i in meses) {
  df_mes = data.frame("Mes"=meses[i], "Anio"=aniosde_cadames_corr[[i]][posiciones_xencima_corr[[i]]], "Acumulado"=datos_xmes_corr[[i]][posiciones_xencima_corr[[i]]])
  tabla_final_corr = rbind(tabla_final_corr, df_mes)
}
tabla_final_corr

# datos pedidos por la consigna para la estacion CORR
cant_veces_xmes_corr
tabla_final_corr
