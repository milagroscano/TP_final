
# a) Leer el archivo y contabilizar la cantidad de datos faltantes para cada mes de ambas
# estaciones. Generar una tabla donde se muestre el detalle de la cantidad de
# faltantes por mes y el anio con mayor cantidad de datos faltantes (sin distincion del mes). 
# Guardar la tabla en un archivo ascii incluyendo un encabezado (mencionando el nombre de la 
# estacion y el periodo de informacion) y las leyendas necesarias.
# Generar un archivo para cada estacion.

# limpio el enviroment
rm(list = ls())

# cargo la libreria para abrir el archivo
library(readxl)
library(gdata)

# ------------------ OCBA ----------------
# abro el archivo que tiene los datos de esta estacion (tuve que cambiar de formato: de xls a csv)
archivo_ocba <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_OCBA_w.csv"
# utilizo read.table para abrir el archivo
datos_ocba = read.table(archivo_ocba, header = F, sep = ",") 
# le asigno nombres a las columnas
colnames(datos_ocba) <- c("cod. de id", "anio", "mes", "pp")
# observo los datos dados
head(datos_ocba) # veo las primeras 6 filas
tail(datos_ocba) # veo las ultimas 6 filas

##########
# Hago los calculos para cada archivo, primero con OCBA y luego con CORR
#########
# defino la variable meses que luego utilizare
meses <- c(1:12)
meses

# calculo la cantidad de datos faltantes por mes, 
# considerar que los datos faltantes son de tres tipos en este caso, los datos NA, los datos = -999.9 y 
# los datos de los meses que no se registraron valores

#### ----- calculo los datos faltantes del tipo NA -------
# busco las posiciones donde hay datos de pp que sean NA 
posiciones_na_ocba <- which(is.na(datos_ocba$pp))
posiciones_na_ocba # no tiene datos NA

# me creo un vector con 12 ceros, que representan la cantidad de datos NA en cada mes
datos_na_ocba <- vector(mode = "numeric", length = 12)
datos_na_ocba

#### calculo el anio con mayor cantidad de datos faltantes de este tipo
# en este caso como no hay datos NA, no existe tal anio

#### ------- calculo los datos faltantes del tipo -999.9 -------
# busco las posiciones donde la pp es igual a -999.9, y luego esas posiciones las indexo con los datos
# para obtener todas las columnas con los datos de esas posiciones
datos_falt_ocba <- datos_ocba[which(datos_ocba$pp== -999.9),]
datos_falt_ocba # puedo ver los meses a los que corresponden los datos faltantes

# me quedo con los datos de los meses y calculo la cantidad que se repite cada mes
# me armo un vector con la cantidad de datos faltantes x mes (faltantes_ocba)
meses_dat_falt_ocba <- datos_falt_ocba[,3]
meses_dat_falt_ocba

faltantes_ocba <- c()
for (i in meses) {
  faltantes_ocba <- c(faltantes_ocba, length(which(meses_dat_falt_ocba==i)))  
}
faltantes_ocba

#### calculo el anio con mayor cantidad de datos faltantes de este tipo sin importar el mes
# me quedo con la columna que tiene los anios 
anios_falt_ocba <- datos_falt_ocba[, 2]
anios_falt_ocba
# defino la variable que tiene el periodo de ocba
anios_ocba <- c(1908:2007)
anios_ocba

cant_x_anio <- c()
for (i in anios_ocba) {
  cant_x_anio <- c(cant_x_anio, length(which(anios_falt_ocba==i)))
}
cant_x_anio

# calculo el valor max de la cantidad de datos faltantes que hay por anio
max(cant_x_anio) # 2
# busco la posicion del anio donde la cantidad es max, para luego hallar el anio
anios_mayor_ocba <- anios_ocba[which(cant_x_anio==2)]
anios_mayor_ocba

#### ------- calculo los datos faltantes que no fueron cargados directamente --------

# primero calculo cuantos datos deberia haber segun los anios cargados y la cantidad de meses
primer_anio_ocba <- datos_ocba$anio[1]
ultimo_anio_ocba <- datos_ocba$anio[length(datos_ocba$anio)]
cant_anios_ocba <- ultimo_anio_ocba-primer_anio_ocba
cant_anios_ocba # 99
# cada anio tiene 12 meses, entonces al ser datos mensuales, debe haber un dato por mes
# si son 99 anios y 12 meses por anio, la cantidad de datos totales debe ser 99*12=1188
# calculo cuantos datos hay en realidad en el archivo
cant_datos_ocba <- length(datos_ocba$mes)
cant_datos_ocba # 1188, aparentemente todos los datos de los meses estan cargados
# deberia darme 1189 ya que en 1908 empieza en el mes 9, y en el 2007 comienza en el mes 9 tambien
# al menos que haya un dato de algun mes que no este cargado y por eso la longitud de datos da bien 
# CHEQUEADO A MANO: hay un mes 7 de 1986 que falta cargar y por eso da el nro 1188 y no 1189

# si se cuantos anios tengo, debo tener la misma cantidad de meses cargados
# en este caso debo tener 99 datos de cada mes
cant_dat_xmes <- c()
for (i in meses) {
  cant_dat_xmes[i] = sum(datos_ocba[,3]==i)
}
cant_dat_xmes
# observo que el mes 7 le falta un dato, y el mes 9 tiene un dato de mas
# lo ultimo se debe a que el primer anio comienza en el mes 9 y el ultimo anio termina en el mismo mes

# me genero un vector donde aparezca que para el mes 7 le falta cargar un dato
datos_no_carg_ocba <- vector(mode="numeric", length = 12)
datos_no_carg_ocba[7] <- 1
datos_no_carg_ocba

# calculo el anio que tuvo mayor cantidad de datos no cargados
j <- 1
cant_dat_xanio <- c()
for (i in anios_ocba) {
  cant_dat_xanio[j] = sum(datos_ocba[,2]==i)
  j <- j + 1
}
cant_dat_xanio
# observo que el primer anio (1908) tiene 4 datos, entonces le falta 8 datos de meses
# y el ultimo anio (2007) tiene 9 datos, entonces le falta 3 datos
# por lo tanto el anio con mayor datos no cargados es el 1908 (considerando el periodo 1908-2007 completo)

## debo discriminar los anios por el tipo de dato faltante, para que al armar la tabla pueda encajar con las filas
# junto todos los anios que obtuve con los datos faltantes en general

# me genero un vector 0 2 0, haciendo referencia a que tal anio tuvo 2 datos faltantes de tipo -999.9
anio_mayor_ocba <- vector(mode = "numeric", length = 3)
anio_mayor_ocba[2] <- 2 
anio_mayor_ocba # se lo asigno a cada anio de anios_mayor_ocba luego en la tabla

# el anio con mayor datos falt no cargados es el 1908
anio_con_dat_no_carg <- 1908

# me genero un vector con la cantidad de datos no cargados para el anio 1908
anio_1908_no_carg <- vector(mode = "numeric", length = 3)
anio_1908_no_carg[3] <- 8 

# me genero un vector con todos los anios que tienen mayor cant de datos faltantes segun el tipo 
vector_anios_dat_falt <- c(anios_mayor_ocba, anio_con_dat_no_carg)
vector_anios_dat_falt

# me creo una matriz con los datos faltantes de todos los tipos
tabla_datos <- matrix(datos_na_ocba, nrow = 1, ncol = 12, byrow = T)
tabla_datos
colnames(tabla_datos) <- meses # le asigno nombres a las columnas
tabla_datos <- rbind(tabla_datos,"Datos faltantes"= faltantes_ocba, "Datos no cargados"=datos_no_carg_ocba)

# me genero otra matriz para los anios con los vectores creados anteriormente
matriz_anios <- matrix(anio_mayor_ocba, nrow = 3, ncol = 5)
colnames(matriz_anios) <- vector_anios_dat_falt
matriz_anios[,5] <- anio_1908_no_carg
matriz_anios

# uno las columnas de la matriz de los anios a la matriz de datos y meses 
tabla_datos_ocba <- cbind(tabla_datos, matriz_anios)
rownames(tabla_datos_ocba) <- c("Datos NA","Datos faltantes","Datos no cargados")
tabla_datos_ocba

#### ARMO ARCHIVO ASCII
# genero el archivo de salida para los datos
archivo_salida_ocba <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/archivo_OCBA" 

# utilizo write para poder escribir el encabezado
write("Estacion: Observatorio Central Buenos Aires (OCBA) 
Periodo: 1908-2007
------------------
La tabla muestra la cantidad de datos que faltan por meses de acuerdo al tipo de dato faltante
Luego, los anios que aparecen son los anios que mayor cantidad de datos faltantes tuvieron
------------------      ",file=archivo_salida_ocba)

# genero el archivo ascii usando la funcion write table, 
# agrego el append para que me agregue el encabezado y traspongo la tabla (queda mejor visualmente asi)  
write.table(t(tabla_datos_ocba), file=archivo_salida_ocba,append = T)

# -------------------- CORR -----------------
# marco la ruta del archivo que tiene los datos de la estacion de corrientes
archivo_corr <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_CORR_w.csv"
# abro el archivo mediante la funcion read.table
datos_corr <- read.table(archivo_corr, header = F, sep = ",")
# le asigno nombres a las columnas de la tabla
colnames(datos_corr) <- c("cod. de id", "anio", "mes", "pp")
head(datos_corr) # veo las primeras 6 filas
tail(datos_corr) # veo las ultimas 6 columnas

#### ----- calculo los datos faltantes del tipo NA ---------
# calculo las posiciones donde hay datos NA
posiciones_na_corr <- which(is.na(datos_corr$pp))
posiciones_na_corr # solo tiene un dato NA en la posicion 18
# defino una variable que tenga los datos de esa posicion
datos_na_corr <- datos_corr[posiciones_na_corr,] 
datos_na_corr # me fijo los datos de esa posicion, mes 3 de 1964

# me creo un vector con 12 ceros, que representan la cantidad de datos NA en cada mes
datos_na_corr <- vector(mode = "numeric", length = 12)
datos_na_corr
# luego le agrego un 1 a la posicion 3 que corresponde al mes 3 donde hay un NA
datos_na_corr[3] <- 1
datos_na_corr

### genero una variable con el anio que tuvo mayor cantidad de datos del tipo NA
# en este caso solo es un anio, ya que solo hay un dato del tipo NA
anio_dat_NA = 1964

#### ---------- calculo los datos faltantes del tipo -999.9 -----------
# busco las posiciones donde hay pp igual a -999.9 y luego me quedo
# con los datos de esas posiciones para averiguar los meses
datos_falt_corr <- datos_corr[which(datos_corr$pp== -999.9),]
datos_falt_corr # puedo ver los meses a los que corresponden los datos faltantes

# me quedo con los datos de los meses y calculo la cantidad que se repite cada mes
meses_dat_falt_corr <- datos_falt_corr[,3]
meses_dat_falt_corr

# me armo un vector con la cantidad de datos faltantes x mes
faltantes_corr <- c()
for (i in meses) {
  faltantes_corr <- c(faltantes_corr, length(which(meses_dat_falt_corr==i)))
}
faltantes_corr

### calculo anio con mayor cantidad de este tipo de dato faltante
# me quedo con los anios de esos datos
anios_falt_corr <- datos_falt_corr[, 2]
anios_falt_corr
# defino una variable con los anios del periodo de corrientes
anios_corr <- c(1961:2007)
anios_corr

cant_x_anio_c <- c()
for (i in anios_corr) {
  cant_x_anio_c <- c(cant_x_anio_c, length(which(anios_falt_corr==i)))
}
cant_x_anio_c

# calculo el valor max de la cantidad de datos faltantes que hay por anio
max(cant_x_anio_c) # 2
# busco la posicion del anio donde la cantidad es max, para luego hallar el anio
anios_mayor_corr <- anios_corr[which(cant_x_anio_c==2)]
anios_mayor_corr

#### ------- calculo los datos faltantes que no fueron cargados directamente --------
# defino el primer y ultimo anio del periodo para calcular la cantidad de anios
primer_anio_corr <- datos_corr$anio[1]
ultimo_anio_corr <- datos_corr$anio[length(datos_corr$anio)]
cant_anios_corr <- ultimo_anio_corr-primer_anio_corr
cant_anios_corr # 46, 12*46 = 552 seria la cantidad de datos que tiene que haber en el periodo
# me fijo cuantos datos tiene en realidad el archivo
cant_datos_corr <- length(datos_corr$mes)
cant_datos_corr # 526, faltan 26 datos que no se ingresaron (datos mensuales)

# si se cuantos anios tengo, debo tener la misma cantidad de meses cargados
# en este caso debo tener 46 datos de cada mes
cant_dat_xmes_c <- c()
for (i in meses) {
  cant_dat_xmes_c[i] = sum(datos_corr[,3]==i)
}
cant_dat_xmes_c # observo que a todos los meses les falta al menos un dato

# chequeo que la cantidad de datos cargados + 26 dan los datos totales que deberia haber
sum(cant_dat_xmes_c) + 26 # si da 552

# me genero un vector con la cantidad de datos no cargados por mes
datos_no_carg_corr <- vector(mode="numeric", length = 12)
for (i in meses) {
  datos_no_carg_corr[i] = 46 - cant_dat_xmes_c[i]
}
datos_no_carg_corr

### debo calcular el anio en el que hubo mayor cantidad de datos faltantes (en este caso en el mes 3, 10 y 11)
j <- 1
cant_dat_xanio_c <- c()
for (i in anios_corr) {
  cant_dat_xanio_c[j] = sum(datos_corr[,2]==i)
  j <- j + 1
}
cant_dat_xanio_c
# observo que el primer anio (1961) solo tiene 2 datos, por lo tanto le faltan 10 datos, pues son 12 meses (anio con mayor cant)
# luego le sigue el anio 2006 con 4 datos, es decir que le faltan 8 datos
# y en tercer lugar el anio 1962 que tiene 6 datos, entonces le faltan 6 datos mensuales

# defino una variable para ese anio con mayor cantidad de datos no cargados, considerando el periodo 1961-2007
anio_con_dat_no_carg_c = anios_corr[1]

## debo discriminar los anios por el tipo de dato faltante, para que al armar la tabla coincida con las filas
# junto todos los anios que obtuve con los datos faltantes en general

# recuerdo el anio con dato NA
anio_dat_NA
# le genero un vector con la cantidad de datos NA
vect_anio_NA = c(1, 0, 0)

# anios con datos del tipo -999.9
anios_mayor_corr
# genero un vector para cada anio con datos -999.9
anio_mayor_corr <- vector(mode = "numeric", length = 3)
anio_mayor_corr[2] <- 2 
anio_mayor_corr # se lo asigno a cada anio de anios_mayor_corr

# el anio con mayor datos falt no cargados es el 1961
anio_con_dat_no_carg_c = 1961

# le asigno al lugar 3 que corresponde a los datos no cargados en la tabla, el valor de datos que le falta
anio_1961_no_carg <- vector(mode = "numeric", length = 3)
anio_1961_no_carg[3] <- 10

# me genero un vector con todos los anios que tienen mayor cant de datos faltantes segun el tipo 
vector_anios_dat_falt_c <- c(anio_dat_NA ,anios_mayor_corr, anio_con_dat_no_carg_c)
vector_anios_dat_falt_c

# me creo una matriz con los datos faltantes de todos los tipos
tabla_datos_corr <- matrix(datos_na_corr, nrow = 1, ncol = 12, byrow = T)
tabla_datos_corr
colnames(tabla_datos_corr) <- meses # le asigno nombres a las columnas
tabla_datos_corr <- rbind(tabla_datos_corr,"Datos faltantes"= faltantes_corr, "Datos no cargados"=datos_no_carg_corr)
tabla_datos_corr

# me genero otra matriz para los anios
matriz_anios_c <- matrix(anio_mayor_corr, nrow = 3, ncol = 8)
colnames(matriz_anios_c) <- vector_anios_dat_falt_c
matriz_anios_c[,1] <- vect_anio_NA
matriz_anios_c[,2:7] <- anio_mayor_corr
matriz_anios_c[,8] <- anio_1961_no_carg
matriz_anios_c

# uno las columnas de la matriz de los anios a la matriz de datos y meses 
tabla_datos_c <- cbind(tabla_datos_corr, matriz_anios_c)
rownames(tabla_datos_c)<-c("Datos NA","Datos faltantes","Datos no cargados")
tabla_datos_c

## ARMO ARCHIVO ASCII
# genero el archivo de salida con los datos
archivo_salida_corr <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/archivo_CORR" 

# luego uso la funcion write para escribir el encabezado del archivo, luego utilizo write.table 
# especificando primero la variable con los datos y despues el nombre de archivo de salida
write("Estacion: Corrientes (CORR) 
Periodo: 1961-2007
------------------
La tabla muestra la cantidad de datos que faltan por meses de acuerdo al tipo de dato faltante
Luego, los anios que aparecen son los anios que mayor cantidad de datos faltantes tuvieron
------------------      ",file=archivo_salida_corr)

write.table(t(tabla_datos_c), file=archivo_salida_corr, quote = T, append = T)
# utilizo append = T para agregar el encabezado, y traspongo la tabla porque queda mejor visualmente