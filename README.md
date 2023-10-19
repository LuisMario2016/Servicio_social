# Indice de sucesión en cambio de uso del suelo
El cambio de uso del suelo se refiere a la transformación de una determinada área geográfica o terreno de un tipo de uso, como agrícola, forestal, o natural, a otro tipo de uso, como urbano, industrial, o comercial. Este proceso implica la modificación de la cobertura y la vegetación del suelo, así como la alteración de las características naturales del entorno.

El índice de sucesión, por otro lado, es una medida utilizada en ecología para evaluar y entender el proceso de cambio en la vegetación y la biodiversidad de un área a lo largo del tiempo. Este índice proporciona información sobre cómo las especies vegetales y animales pueden cambiar y evolucionar en respuesta a disturbios naturales o actividades humanas, como incendios forestales, tala de árboles o restauración de áreas degradadas. Al analizar el índice de sucesión, los ecólogos pueden obtener una mejor comprensión de cómo los ecosistemas se recuperan o se transforman después de disturbios, y cómo se pueden tomar medidas para conservar o restaurar la biodiversidad y la salud de los ecosistemas.
##### Todos los datos seran manejados en el software R
## Obteniendo los datos
Los datos trabajados se pueden obtener desde el portal [Land-Use-Harmonization](https://luh.umd.edu/ "Land-use-harmonization") , en su version [LUH2 v2h Release (10/14/16)](https://luh.umd.edu/LUH2/LUH2_v2h/states.nc "LUH2 v2h Release (10/14/16)"), la cual contiene datos historicos de el año 850 hasta el año 2015.
## Carga y visualizacion de los datos
Como primera accion, definiremos el directorio de trabajo y cargamos todas las paqueterias necesarias para el analisis
~~~
setwd("C:/serviciosocial/DATOS")

library(ncdf4)      # package for netcdf manipulation
library(raster)      # package for raster manipulation
library(sf)            # package for geospatial analysis
library(ggplot2)  #package for 
library(rpart.plot) #package for 
library (rpart)      #package for
library (sp)          #package for
~~~
Abrimos el archivo "states" para poder obtener las variables, en este caso utilizamos la banda 1165 (año 2015) para reducir la cantidad de datos.
~~~
sts<-nc_open("states.nc")
sts #Visualizar las propiedades, variables, y documentacion de nuestro archivo

rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.primn_2015 <-raster("states.nc", var="primn",band=1165)
rasX.secdf_2015 <-raster("states.nc", var="secdf",band=1165)
rasX.secdn_2015 <-raster("states.nc", var="secdn",band=1165)
rasX.urban_2015 <-raster("states.nc", var="urban",band=1165)
rasX.c3ann_2015 <-raster("states.nc", var="c3ann",band=1165)
rasX.c4ann_2015 <-raster("states.nc", var="c4ann",band=1165)
rasX.c3per_2015 <-raster("states.nc", var="c3per",band=1165)
rasX.c4per_2015 <-raster("states.nc", var="c4per",band=1165)
rasX.c3nfx_2015 <-raster("states.nc", var="c3nfx",band=1165)
rasX.pastr_2015 <-raster("states.nc", var="pastr",band=1165)
rasX.range_2015 <-raster("states.nc", var="range",band=1165)
rasX.secmb_2015 <-raster("states.nc", var="secmb",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)
~~~
Para visualizar cualquier variable y conocer un resumen de los datos, se utiliza:
~~~
plot(rasX.VARIABLE INTERES_2015)
~~~
## condicional ¿que titulo
Los datos registrados en la variable "secma" (secondary mean age), tienen una representación 
Por lo que se tiene 
~~~
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)

max.secma<- cellStats(rasX.secma_2015,stat = "max", na.rm = TRUE)
rasX.secma_2015[rasX.PFYNF == 1]<-max.secma
plot(rasX.secma_2015)
~~~
