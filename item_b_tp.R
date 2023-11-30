
# b) Calcular la media y el desvio estandar para cada mes y presentarlo en un grafico
# para cada estacion en la misma figura. En el grafico debe estar la linea que
# representa a la media y las 2 lineas que representan al desvio sumado y restado a la media.

###### ---------- OCBA -----------
rm(list = ls())

# abro el archivo, defino la ruta del archivo
archivo_ocba <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_OCBA_w.csv"
# utilizo read.table para abrir el archivo
datos_ocba = read.table(archivo_ocba, header = F, sep = ",") 
# le asigno nombres a las columnas de la tabla
colnames(datos_ocba) <- c("cod. de id", "anio", "mes", "pp")
head(datos_ocba) # veo las primeras 6 filas
tail(datos_ocba) # veo las ultimas 6 filas

# defino la variable de meses para luego utilizarlo en los calculos
meses <- c(1:12)

# para calcular la media y el desvio estandar a los datos faltantes del tipo -999.9 los asigno como NA
datos_ocba$pp[which(datos_ocba$pp==-999.9)] <- NA

# luego debo quedarme con los datos de cada mes por separado, me creo una lista 
# donde cada contenedor sea un mes con sus datos
datos_xmes_ocba <- list()
for (i in meses) {
  datos_xmes_ocba[[i]] <- datos_ocba$pp[which(datos_ocba$mes==i)]
}
datos_xmes_ocba # observo que se haya asignado a cada contenedor los datos correspondiente a cada mes

# por medio de lapply puedo calcular la media y desvio estandar para cada contenedor de la lista, 
# lapply devuelve listas, con na.rm = T no tengo en cuenta los valores NA
medias_ocba = lapply(datos_xmes_ocba, function(x) mean(x, na.rm = T))
medias_ocba

desvios_ocba = lapply(datos_xmes_ocba, function(x) sd(x, na.rm = T))
desvios_ocba

# me genero vectores con los datos del desvio y la media para luego crear un df, ya que ggplot acepta data frames
vector_medias_ocba = c()
vector_desvios_ocba = c()
for (i in meses) {
  vector_medias_ocba = c(vector_medias_ocba, medias_ocba[[i]]) 
  vector_desvios_ocba = c(vector_desvios_ocba, desvios_ocba[[i]])
}
vector_medias_ocba
vector_desvios_ocba

## con los datos del desvio, me genero otros dos vectores con el desvio sumado y restado la media
# genero los datos del desvio + media
desvios_sumados_ocba = c()
for (i in meses) {
  desvios_sumados_ocba[i] = vector_medias_ocba[i] + vector_desvios_ocba[i]
}
desvios_sumados_ocba

# genero los datos del desvio - media
desvios_restados_ocba = c()
for (i in meses) {
  desvios_restados_ocba[i] = vector_medias_ocba[i] - vector_desvios_ocba[i]
}
desvios_restados_ocba

# ggplot necesita data frames como archivo para poder generar el grafico, asi que me creo 3 data frames,
# uno para las medias y otros dos para el desvio sumado y restado la media
# para luego poder unirlos en un df y usarlo para generar el grafico

# los datos de las medias los convierto en un df para luego poder graficar
df_medias_ocba = data.frame("Datos"=vector_medias_ocba) # creo un df con los valores de la media
df_medias_ocba$meses <- meses # creo una columna nueva al df con los valores de los meses
df_medias_ocba$tipo <- rep("media",12) # creo una columna con el "tipo" de dato que estoy cargando, 
                                       # lo repito la cantidad de veces que necesito que aparezca en la columna

# las columnas creadas meses y tipo van a servir para luego poder unir los 3 df,
# ya que cada df va a tener el mismo nombre de columnas

# con los vectores generados de los desvios sumados y restados, me armo dos df para luego armar los graficos
df_desv_sum_ocba = data.frame("Datos"=desvios_sumados_ocba)
df_desv_sum_ocba$meses <- meses
df_desv_sum_ocba$tipo <- rep("desv-sum",12)

df_desv_res_ocba = data.frame("Datos"=desvios_restados_ocba)
df_desv_res_ocba$meses <- meses
df_desv_res_ocba$tipo <- rep("desv-res",12)

# luego para crear el grafico con las tres lineas, necesito un df que tenga los 3 df unidos por filas
df_ocba <- rbind(df_medias_ocba, df_desv_res_ocba, df_desv_sum_ocba)
df_ocba
# de esta forma ggplot interpreta mejor los tipos de datos


###### -------- CORR ------------

# defino la ruta del archivo para luego abrir el archivo
archivo_corr <- "/LIANA/Escritorio/LicAtmosfera/Laboratorio de Procesamiento de Información Meteorológica/2C 2023/Clases Pract/datos_TP_final/datos_CORR_w.csv"
# abro el archivo con la funcion read.table
datos_corr <- read.table(archivo_corr, header = F, sep = ",")
# le asigno nombres a las columnas de la tabla
colnames(datos_corr) <- c("cod. de id", "anio", "mes", "pp")
head(datos_corr) # veo las primeras 6 filas
tail(datos_corr) # veo las ultimas 6 filas

# le asigno NA a los datos faltantes
datos_corr$pp[which(datos_corr$pp==-999.9)] <- NA
tail(datos_corr) # chequeo que se hayan asignado correctamente

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
vector_medias_corr
vector_desvios_corr

## con los datos del desvio, me genero otros dos vectores con el desvio sumado y restado la media
# genero los datos del desvio + media
desvios_sumados_corr = c()
for (i in meses) {
  desvios_sumados_corr[i] = vector_medias_corr[i] + vector_desvios_corr[i]
}
desvios_sumados_corr

# genero los datos del desvio - media
desvios_restados_corr = c()
for (i in meses) {
  desvios_restados_corr[i] = vector_medias_corr[i] - vector_desvios_corr[i]
}
desvios_restados_corr

# ggplot necesita data frames como archivo para poder generar el grafico,
# asi que me creo 3 data frames, uno para las medias y otros dos para el desvio sumado y restado la media

# los datos de las medias los convierto en un df para luego poder graficar
df_medias_corr = data.frame("Datos"=vector_medias_corr) # creo un df con los valores de la media
df_medias_corr$meses <- meses # creo una columna nueva al df con los valores de los meses
df_medias_corr$tipo <- rep("media",12) # creo una columna con el "tipo" de dato que estoy cargando

# con los vectores generados de los desvios sumados y restados, me armo dos df para luego armar los graficos
df_desv_sum_c = data.frame("Datos"=desvios_sumados_corr)
df_desv_sum_c$meses <- meses
df_desv_sum_c$tipo <- rep("desv-sum",12)

df_desv_res_c = data.frame("Datos"=desvios_restados_corr)
df_desv_res_c$meses <- meses
df_desv_res_c$tipo <- rep("desv-res",12)

# luego para crear el grafico con las tres lineas, necesito un df que tenga los 3 df unidos por filas
df_corr <- rbind(df_medias_corr, df_desv_res_c, df_desv_sum_c)
df_corr
# de esta forma ggplot interpreta mejor los tipos de datos


### una vez que ya tengo mis df de cada estacion con sus datos correspondientes, 
# debo crear un grafico con dos paneles donde cada uno tenga el grafico de las lineas que pide el enunciado
# para eso me creo un df nuevo donde tenga los datos de ambas estaciones, utilizo una columna llamada estacion
df_ocba$Estacion=rep("OCBA", length(df_ocba$Datos))
df_corr$Estacion=rep("CORR",length(df_corr$Datos))
df_estaciones<- rbind(df_ocba, df_corr)
df_estaciones
# una vez que ya tengo los datos calculados procedo a armar los graficos

# cargo las librerias a utilizar 
library(ggplot2) 
library(lubridate)

# genero el grafico con las 3 lineas de las medias, el desvio + media y el desvio - media, para cada mes
grafico_lineas = ggplot(df_estaciones,mapping=aes(x=meses, y = Datos,color=tipo)) + # en el eje y le especifico los valores
  # del eje que quiero que tome, en este caso es la columna de todos los datos de los tres vectores de datos
  # color = tipo, me asigna un color por cada tipo que encuentre distinto en la columna tipo
  geom_line(size=1) + # asigna que el grafico sea con lineas, size me especifica el grosor de la linea
  scale_color_manual(values = c("limegreen","forestgreen","black")) + # le doy color a las lineas generadas
  geom_point(size=2) + # asigna puntos a los valores para cada mes sobre la linea, size especifica el tamaño
  scale_x_continuous(breaks=c(1:12),labels = month(1:12,label = T)) + # breaks: cada cuanto quiero que me muestre el valor asignado al eje x
  # labels, asigna nombres a los valores del eje, en este caso con month y label = T obtengo los nombres de los meses
  labs(title="Precipitacion media mensual", # agrega titulos
       x="Meses", y="Precipitacion(mm)") + # agrega subtitulos, y nombres a cada eje
  facet_wrap(.~Estacion) + # facet_wrap es para generar paneles, en este caso los genera a partir de la cantidad del tipo de la columna Estacion
  theme_bw() # depende del theme que elija cmabia el fondo de los paneles

grafico_lineas
