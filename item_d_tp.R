
# d) Calcular los valores de acumulado anual para todo el periodo de estudio y presentar
# en barras los valores anuales y, en linea negra el promedio de todo el periodo.
# Presentar y almacenar un grafico para cada estacion.

#### ------------------- OCBA ---------------------
# limpio el environment
rm(list = ls())
# genero la ruta del archivo para luego poder abrirlo
archivo_ocba <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_OCBA_w.csv" 
# abro el archivo con la funcion read.table
datos_ocba = read.table(archivo_ocba, header = F, sep = ",")  
# le asigno nombres a sus columnas
colnames(datos_ocba) <- c("cod. de id", "anio", "mes", "pp") 
head(datos_ocba) # veo las primeras 6 filas 
tail(datos_ocba) # veo las ultimas 6 filas

# defino la variable anios para la estacion, determinando el periodo
anios_ocba <- c(1908:2007) 

# le asigno NA a los datos faltantes del tipo -999.9 para luego poder hacer calculos, sin tener en cuenta los NA
datos_ocba$pp[which(datos_ocba$pp==-999.9)] <- NA 
head(datos_ocba) # verifico que se haya ejecutado bien el codigo

# calculo el acumulado por anio y genero un vector para los resultado, 
# donde cada posicion del vector representa un anio del periodo
acumulado_anual_ocba = c()
for (i in 1:length(anios_ocba)) {
  posiciones_anio = which(datos_ocba$anio==anios_ocba[i])
  pp_anio = datos_ocba$pp[posiciones_anio]
  acumulado_anio = sum(pp_anio, na.rm = T)
  acumulado_anual_ocba = c(acumulado_anual_ocba, acumulado_anio)
}
acumulado_anual_ocba # es un vector donde cada posicion representa un anio, y el valor es su acumulado
# a partir de los valores del acumulado anual calculo el promedio sobre el periodo
promedio_ocba = mean(acumulado_anual_ocba) 

# debo generarme un df con los datos para armar el grafico
df_acumulado_ocba = data.frame("Anio"= anios_ocba, "Acumulado"= acumulado_anual_ocba, "Promedio"=rep(promedio_ocba, length(anios_ocba)))

# cargo las librerias a utilizar para el grafico
library(ggplot2) 
library(lubridate)

# genero el grafico pedido con el acumulado anual y el promedio 
barras_ocba = ggplot(df_acumulado_ocba,mapping=aes(x=anios_ocba, y = acumulado_anual_ocba)) + # en el eje y le especifico los valores 
  # del eje que quiero que tome, en este caso es la columna de acumulado
  geom_col() + # asigna que el grafico sea con columnas
  geom_bar(stat="identity" ,color = "black", fill = "springgreen4") + # le doy color al borde de las barras con color, y fill le da color al relleno de las barras
  geom_line(mapping = aes(x=anios_ocba, y = promedio_ocba), color="black", size=1) + # asigna lineas al grafico, 
  # en este caso el valor del promedio, size agranda la linea
  scale_x_continuous(breaks=seq(1908, 2007, 10)) + # breaks, cada cuanto quiero que me muestre el valor asignado al eje x
  labs(title="Acumulado anual de pp y su promedio", # agrega titulos y subtitulos, y asigna nombres a los ejes
       subtitle="Estacion: OCBA", x="Anios", y="Precipitacion(mm)")
barras_ocba # lo visualizo 

#### --------------- CORR ---------------------
# genero la ruta para el archivo con los datos de la estacion de corrientes
archivo_corr <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_CORR_w.csv"
# abro el archvio a traves de la funcion read.table
datos_corr <- read.table(archivo_corr, header = F, sep = ",")
# a la tabla con los datos le asigno nombres a sus columnas
colnames(datos_corr) <- c("cod. de id", "anio", "mes", "pp")

# defino el periodo de anios para la estacion CORR
anios_corr <- c(1961:2007)

# le asigno NA a los datos faltantes
datos_corr$pp[which(datos_corr$pp==-999.9)] <- NA
tail(datos_corr) # verifico que se haya asignado bien

# calculo el acumulado anual para cada anio del periodo, y el promedio del periodo en total
acumulado_anual_corr = c()
for (i in 1:length(anios_corr)) {
  posiciones_anio = which(datos_corr$anio==anios_corr[i])
  pp_anio = datos_corr$pp[posiciones_anio]
  acumulado_anio = sum(pp_anio, na.rm = T)
  acumulado_anual_corr = c(acumulado_anual_corr, acumulado_anio)
}
acumulado_anual_corr # es un vector donde cada posicion representa un anio, y el valor es su acumulado
promedio_corr = mean(acumulado_anual_corr) # es un valor del promedio de todo el periodo, promedio sobre el acumulado anual

# debo generarme un df con los datos para amar el grafico
df_acumulado_corr = data.frame("Anio"= anios_corr, "Acumulado"= acumulado_anual_corr, "Promedio"=rep(promedio_corr, length(anios_corr)))

# cargo las librerias a utilizar 
library(ggplot2) 
library(lubridate)

# genero el grafico pedido con el acumulado por anio y el promedio del periodo 
barras_corr = ggplot(df_acumulado_corr,mapping=aes(x=anios_corr, y = acumulado_anual_corr)) + # en el eje y le especifico los valores 
  # del eje que quiero que tome, en este caso es la columna de acumulado
  geom_col() + # asigna que el grafico sea con columnas
  geom_bar(stat="identity" ,color = "black", fill = "seagreen4") + # le doy color al borde de las columnas con color y con fill le doy color al relleno de las columnas
  geom_line(mapping = aes(x=anios_corr, y = promedio_corr), color="black", size=1) + # asigna lineas al grafico, 
  # en este caso se toma el valor del promedio para que genere la linea negra, size sirve para engrosar la linea
  scale_x_continuous(breaks=seq(1961, 2007, 5)) + # breaks, cada cuanto quiero que me muestre el valor asignado al eje x
  labs(title="Acumulado anual de pp y su promedio", # agrega titulos
       subtitle="Estacion: CORR", x="Anios", y="Precipitacion(mm)")
barras_corr
