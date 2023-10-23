# Indice de sucesión en cambio de uso del suelo
El cambio de uso del suelo se refiere a la transformación de una determinada área geográfica o terreno de un tipo de uso, como agrícola, forestal, o natural, a otro tipo de uso, como urbano, industrial, o comercial. Este proceso implica la modificación de la cobertura y la vegetación del suelo, así como la alteración de las características naturales del entorno.

El índice de sucesión, por otro lado, es una medida utilizada en ecología para evaluar y entender el proceso de cambio en la vegetación y la biodiversidad de un área a lo largo del tiempo. Este índice proporciona información sobre cómo las especies vegetales y animales pueden cambiar y evolucionar en respuesta a disturbios naturales o actividades humanas, como incendios forestales, tala de árboles o restauración de áreas degradadas. Al analizar el índice de sucesión, los ecólogos pueden obtener una mejor comprensión de cómo los ecosistemas se recuperan o se transforman después de disturbios, y cómo se pueden tomar medidas para conservar o restaurar la biodiversidad y la salud de los ecosistemas.
##### Todos los datos seran manejados en el software R
<img src="https://3.bp.blogspot.com/-yau995nEnw0/Uyr0XN9kXGI/AAAAAAAAAMo/LO8KN2bFzfQ/s1600/sucesiones-ciencias7_1477.jpg" width="50%")
Proceso de sucecion ecologica, donde el primero es la perturbacion, y al ultimo una recuperacion del ecosistema en cuestion, pasando por diversas etapas, por ejemplo, la aparicion de plantas pioneras, plantas madre, etc.
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
rasX.secma_2015[rasX.primf_2015 == 1]<-max.secma
plot(rasX.secma_2015)
~~~
[![Secma corregido](https://github.com/LuisMario2016/Servicio_social/blob/main/secmacorregido.png "Secma corregido")](https://raw.githubusercontent.com/LuisMario2016/Servicio_social/main/secmacorregido.png?token=GHSAT0AAAAAACIFWB37ROCMZBXPLXRUUQKMZJV4G3A "Secma corregido")
## Seleccion de la variable predictora
Para conocer acerca de como estos datos de secma, se realiza un arbol de clasificacion con la ayuda de la libreria [Rpart](https://www.rdocumentation.org/packages/rpart/versions/4.1.21/topics/rpart "Rpart"), esto con el fin de averiguar donde es que estos valores estan clasificados. Para ello como priemer paso, debemos crear dos variables que nos representen la cobertura total, es decir vegetacion primaria foretal mas la no forestal, asi como la vegetacion secundaria forestal y no forestal.
~~~
rasX.PFYNF<- rasX.primf_2015+rasX.primn_2015 #Vegetacion primaria forestal+ no forestal
rasX.SFYNF<- rasX.secdf_2015+rasX.secdn_2015 #Vegetacion secundarias forestal + no forestal
~~~
Creamos una variable clasificadora para nuestro arbol, en este caso es el 4 cuatil de los datos contenidos en secma, ya que estos son las edades mas recientes de perturbacion
~~~
cuartil_4 <- quantile(values(rasX.secma_2015), probs = 0.75, na.rm=T)
mascara_cuartil_4 <- rasX.secma_2015 > cuartil_4
plot(mascara_cuartil_4)
~~~
Como el paquete Rpart no nos deja trabajar directamente con variables tipo raster, se debe convertir los datos a un data frame, entonces cada variable es convertida a un data frame, y posterior se unen todas en un solo data frame.
~~~
df_primf<-as.data.frame(rasX.primf_2015)
df_primn<-as.data.frame(rasX.primn_2015)
df_secdf<-as.data.frame(rasX.secdf_2015)
df_secdn<-as.data.frame(rasX.secdn_2015)
df_secma<-as.data.frame(rasX.secma_2015)
df_urban<-as.data.frame(rasX.urban_2015)
df_c3ann<-as.data.frame(rasX.c3ann_2015)
df_c4ann<-as.data.frame(rasX.c4ann_2015)
df_c3per<-as.data.frame(rasX.c3per_2015)
df_c4per<-as.data.frame(rasX.c4per_2015)
df_c3nfx<-as.data.frame(rasX.c3nfx_2015)
df_pastr<-as.data.frame(rasX.pastr_2015)
df_range<-as.data.frame(rasX.range_2015)
df_PFYNF<- as.data.frame(rasX.PFYNF)
df_mascara_4<-as.data.frame(mascara_cuartil_4)

df_states_rasters<- data.frame(df_primf,df_primn,df_secdf,df_secdn,df_secma,df_urban,
                               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,
                               df_PFYNF, df_mascara_4)
~~~
Para construir el arbol utilizamos el metodo "class",  seleccionamos como clase a el cuartil 4 de los datos contenidos en secma (layer.1), y como variables predictoras a las demas (urban.land, prennial, c3.annual,crops, c4.annual,crops...)
~~~
Install.packages("Rpart") #Solo si no cuenta con la librerias
install.packages("rpart.plot") 

library(rpart.plot)
library (rpart) #para una visualizacion mas amigable del árbol
modelo_cuartil_4<- rpart(layer.1~
                 + potentially.forested.secondary.land+
                 potentially.non.forested.secondary.land+
                 C3.annual.crops+ C4.annual.crops+ C4.perennial.crops+
                 C3.perennial.crops+C3.nitrogen.fixing.crops+
                 managed.pasture+ rangeland+
                 C3.annual.crops+ urban.land+ forested.primary.land+
				 non.forested.primary.land
               ,data = df_states_rasters, method = "class")

rpart.plot(modelo_cuartil_4)
~~~
[![Arbol de decision](https://github.com/LuisMario2016/Servicio_social/blob/main/arbol%20cuartil%204.png "Arbol de decision")](https://raw.githubusercontent.com/LuisMario2016/Servicio_social/main/arbol%20cuartil%204.png?token=GHSAT0AAAAAACIFWB36H4IQRPONYBI2SQN6ZJV5I4A "Arbol de decision")


