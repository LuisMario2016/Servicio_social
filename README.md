## Proyecto "2023-12/186-357 UNAM" de servicio social titulado: "Aprendizaje de máquina sobre el Impacto del Cambio en el Uso del Suelo sobre la Biodiversidad y el Bienestar"

------------
#### Codigo escrito por: Paz Villagarcia Luis Mario 
#### mail: luis173001@comunidad.unam.mx
## Resumen
## Justificación
En años recientes se ha notado un incremento global en la temperatura de la tierra, lo que causa perdidas económicas derivadas de tal efecto, una de las causas principales de que esto suceda es el efecto invernadero, el cual incrementa con la modificación de la vegetación natural, estas modificaciones pueden ser de distintos tipos; tala, establecimiento de urbes, construcción de carreteras, establecimiento de tierras de cultivo intensivo, entre otras. Todas estas actividades han incrementado a partir de la primera revolución industrial, según el país, una o todas han desarrollado un alza de acuerdo a distintos periodos, por lo que conocer como se comportaron estas variables son una buena explicación de las consecuencias actuales a ciertos problemas como epidemias, perdida de biodiversidad o la perdida de fertilidad de los suelos. A su vez, conocer estos datos nos ayuda a predecir como podrían comportarse las variables de acuerdo nuestro crecimiento poblacional y a como se desarrollan las actividades económicas, brindado una herramienta para la planificación de un desarrollo sustentable, según la agenda 2030, a la que tanto México entre otros países forman parte.
## Objetivos
- Elaborar un índice de sucesión utilizando machine learning para conocer el estado/condición de uso de suelo respecto a datos geoespaciales.
- Comprender la proporción del cambio de las variables de cambio de uso de suelo de países de gran crecimiento poblacional y de actividad económica.
- Crear un modelo lineal de predicción del comportamiento de las variables para 2100 para 10 países con gran crecimiento económico.

# Indice de sucesión en cambio de uso del suelo
El cambio de uso del suelo se refiere a la transformación de una determinada área geográfica o terreno de un tipo de uso, como agrícola, forestal, o natural, a otro tipo de uso, como urbano, industrial, o comercial. Este proceso implica la modificación de la cobertura y la vegetación del suelo, así como la alteración de las características naturales del entorno.

El índice de sucesión, por otro lado, es una medida utilizada en ecología para evaluar y entender el proceso de cambio en la vegetación y la biodiversidad de un área a lo largo del tiempo. Este índice proporciona información sobre cómo las especies vegetales y animales pueden cambiar y evolucionar en respuesta a disturbios naturales o actividades humanas, como incendios forestales, tala de árboles o restauración de áreas degradadas. Al analizar el índice de sucesión, los ecólogos pueden obtener una mejor comprensión de cómo los ecosistemas se recuperan o se transforman después de disturbios, y cómo se pueden tomar medidas para conservar o restaurar la biodiversidad y la salud de los ecosistemas.
##### Todos los datos seran manejados en el software R
[![Sucesion ecologica](https://3.bp.blogspot.com/-yau995nEnw0/Uyr0XN9kXGI/AAAAAAAAAMo/LO8KN2bFzfQ/s1600/sucesiones-ciencias7_1477.jpg "Sucesion ecologica")](https://3.bp.blogspot.com/-yau995nEnw0/Uyr0XN9kXGI/AAAAAAAAAMo/LO8KN2bFzfQ/s1600/sucesiones-ciencias7_1477.jpg "Sucesion ecologica")
###### Proceso de sucesión ecológica, donde el primer suceso es la perturbación, y el último es una recuperación del ecosistema en cuestión, pasando por diversas etapas, por ejemplo, la aparición de plantas pioneras, plantas madre, etc.

------------


## Obteniendo los datos
Los datos trabajados se pueden obtener desde el portal [Land-Use-Harmonization](https://luh.umd.edu/ "Land-use-harmonization") , en su version [LUH2 v2h Release (10/14/16)](https://luh.umd.edu/LUH2/LUH2_v2h/states.nc "LUH2 v2h Release (10/14/16)"), la cual contiene datos historicos de el año 850 hasta el año 2015.
## Carga y visualizacion de los datos
Como primera acción, definiremos el directorio de trabajo y cargamos todas las paqueterías necesarias para el análisis
~~~
setwd("C:/serviciosocial/DATOS")

library(ncdf4)      # package for netcdf manipulation
library(raster)      # package for raster manipulation
library(sf)            # package for geospatial analysis
library(rpart.plot) #package for visualization of classification tree
library (rpart)      #package for method of classification tree
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
## Correccion y ajuste de indice "SECMA"
Los datos registrados en la variable "secma" (secondary mean age), tienen una representación en años, donde los valores más altos serían un sitio que ya paso la etapa sucesional, es decir, ya está restaurado, de igual forma en "primf" la etapa de desarrollo de un bosque se mide de 0 a 1 siendo 0 un bosque establecido y 1 un bosque de edad joven.
Por lo tanto, a manera de asignación, mediante un condicional, establecemos que los valores máximos de secma sean asignados a los valores =1 en primf, confirmando que un valor alto de secma es un sitio que ha estado sufriendo cambios, llegando al establecimiento.
~~~
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)

max.secma<- cellStats(rasX.secma_2015,stat = "max", na.rm = TRUE)
rasX.secma_2015[rasX.primf_2015 == 1]<-max.secma
plot(rasX.secma_2015)
~~~
[![Secma corregido](https://github.com/LuisMario2016/Servicio_social/blob/main/secmacorregido.png "Secma corregido")](https://raw.githubusercontent.com/LuisMario2016/Servicio_social/main/secmacorregido.png?token=GHSAT0AAAAAACIFWB37ROCMZBXPLXRUUQKMZJV4G3A "Secma corregido")
## Creacion de nuevas variables de interes
Para conocer acerca de cómo estos datos de secma, se realiza un árbol de clasificación con la ayuda de la librería [Rpart](https://www.rdocumentation.org/packages/rpart/versions/4.1.21/topics/rpart "Rpart"), esto con el fin de averiguar dónde es que estos valores están clasificados. Para ello, como primer paso, debemos crear dos variables que nos representen la cobertura total, es decir, vegetación primaria forestal más la no forestal, así como la vegetación secundaria forestal y no forestal. Esto con la finalidad de poder tener un panorama completo acerca de ambas coberturas.
~~~
rasX.PFYNF<- rasX.primf_2015+rasX.primn_2015 #Vegetacion primaria forestal+ no forestal
rasX.SFYNF<- rasX.secdf_2015+rasX.secdn_2015 #Vegetacion secundarias forestal + no forestal
max.secma<- cellStats(rasX.secma_2015,stat = "max", na.rm = TRUE)
rasX.secma_2015[rasX.FYNF_2015 == 1]<-max.secma
~~~
###  Construyendo el árbol de decisión

Como el paquete Rpart no nos deja trabajar directamente con variables tipo raster, se debe convertir los datos a un data frame, entonces cada variable es convertida a un data frame, y posteriormente se unen todas en un solo data frame.
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
                               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,df_secmb, df_FYNF)
							   
df_states<- na.exclude(df_states_rasters)
~~~
Esta línea de código con ayuda de la función cut, proporciona la variable secma en sus cuatro cuartiles, para poder utilizar esta nueva variable en el clasificador.
~~~
y= cut(df_states$secondary.mean.age,quantile(df_states$secondary.mean.age), include.lowest =T)
y<-as.integer(y)
df_states$cuartiles=y
~~~
Para construir el árbol utilizamos el método "class",  seleccionamos como clase a la variable cuartiles, y como variables predictoras a las demás (urban.land, prennial, c3.annual,crops, c4.annual,crops...)
~~~
modelo_multicuartil <- rpart(cuartiles ~ 
                               potentially.forested.secondary.land + 
                               potentially.non.forested.secondary.land + 
                               C3.annual.crops + C4.annual.crops + 
                               C4.perennial.crops + C3.perennial.crops + 
                               C3.nitrogen.fixing.crops + managed.pasture + 
                               rangeland + C3.annual.crops + urban.land + 
                               forested.primary.land + non.forested.primary.land,
                             data = df_states, method = "class")
rpart.plot(modelo_multicuartil, extra = 0, cex = 0.7, uniform = TRUE, space=0.0000000000000001)
~~~
[![arbol](https://raw.githubusercontent.com/LuisMario2016/Servicio_social/main/multiclase_clasificador.png?token=GHSAT0AAAAAACM3HEN3MACE2O5YSBKT72LYZNPI7PA "arbol")](https://github.com/LuisMario2016/Servicio_social/blob/main/multiclase_clasificador.png "arbol")
