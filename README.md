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
