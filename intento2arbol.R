################################################################################
setwd("C:/serviciosocial/DATOS")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(sf) # package for geospatial analysis
library(ggplot2)
################################################################################
sts<-nc_open("states.nc")
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

rasX.FYNF_2015<- rasX.primf_2015+rasX.primn_2015 ## variable respuesta
str(rasX.primf_2015)


df_primf<-as.data.frame(rasX.primf_2015)
df_primn<-as.data.frame(rasX.primn_2015)
df_secdf<-as.data.frame(rasX.secdf_2015)
df_secdn<-as.data.frame(rasX.secdn_2015)
df_secma<-as.data.frame(rasX.secma_2015)
df_urban<-as.data.frame(rasX.urban_2015)
df_c3ann<-as.data.frame(rasX.c3ann_2015)
df_c4ann<-as.data.frame(rasX.c4ann_2015)#
df_c3per<-as.data.frame(rasX.c3per_2015)
df_c4per<-as.data.frame(rasX.c4per_2015)
df_c3nfx<-as.data.frame(rasX.c3nfx_2015)
df_pastr<-as.data.frame(rasX.pastr_2015)
df_range<-as.data.frame(rasX.range_2015)
df_secmb<-as.data.frame(rasX.secmb_2015)
df_FYNF<- as.data.frame(rasX.FYNF_2015)


df_states_rasters<- data.frame(df_primf,df_primn,df_secdf,df_secdn,df_secma,df_urban,
               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,df_secmb,
               df_FYNF)##<- transformar a un special object de sp
install.packages("rpart.plot")
library(rpart.plot)
library (rpart)
modelo<- rpart(clase~
               + potentially.forested.secondary.land+
                 potentially.non.forested.secondary.land+
                 C3.annual.crops+ C4.annual.crops+ C4.perennial.crops+
                 C3.perennial.crops+C3.nitrogen.fixing.crops+
                 managed.pasture+ rangeland+ secondary.mean.biomass.carbon.density+
               C3.annual.crops+  urban.land + secondary.mean.age
                 ,data = df_states_rasters, method = "class")
modelo2<- rpart(clase~secondary.mean.age
               ,data = df_states_rasters, method = "class")

df_states_rasters$clase=clase

rpart.plot(modelo2)

df_states_rasters$x1= rep(1,length(clase)) 

clase=as.character(df_states_rasters$layer==1)
noclase=df_states_rasters$layer<1
table(clase)
table(noclase)

#################################################################### GRAFICAS
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)
rasX.secdf_2015 <-raster("states.nc", var="secdf",band=1165)
rasX.ForSec<- rasX.primf_2015+rasX.secdf_2015

###Transformar a df y unirlos

df_primf<-as.data.frame(rasX.primf_2015)
df_secdf<-as.data.frame(rasX.secdf_2015)
df_secma<-as.data.frame(rasX.secma_2015)
df_ForSec<- as.data.frame(rasX.ForSec)

todos<- cbind(df_primf,df_secdf,df_secma,df_ForSec)

##1 ) grafica de secma vs cobertura de bosque primario
ggplot(todos, aes(x = forested.primary.land, y = secondary.mean.age)) +
  geom_point(aes(color = "X"), size = 3) +   # Color para x
  geom_point(aes(color = "Y"), size = 3) +   # Color para y
  labs(title = "Scatter Plot: forested.primary.land vs. secondary.mean.age",
       x = "forested.primary.land",
       y = "secondary.mean.age") +
  scale_color_manual(values = c("X" = "black", "Y" = "blue")) +
  guides(color = guide_legend(title = "Variables"))


##2 ) grafica de secma vs cobertura de bosque secundario
ggplot(todos, aes(x = potentially.forested.secondary.land, y = secondary.mean.age)) +
  geom_point(aes(color = "X"), size = 1) +   # Color para x
  geom_point(aes(color = "Y"), size = 3) +   # Color para y
  labs(title = "Scatter Plot: forested.primary.land vs. secondary.mean.age",
       x = "forested.primary.land",
       y = "secondary.mean.age") +
  scale_color_manual(values = c("X" = "black", "Y" = "brown")) +
  guides(color = guide_legend(title = "Variables"))


##3 ) grafica de secma vs cobertura de bosque primario + cobertura de bosque secundario
ggplot(todos, aes(x = layer, y = secondary.mean.age)) +
  geom_point(aes(color = "X"), size = 3) +   # Color para x
  geom_point(aes(color = "Y"), size = 3) +   # Color para y
  labs(title = "Scatter Plot: forested.primary.land vs. secondary.mean.age",
       x = "forested.primary.land",
       y = "secondary.mean.age") +
  scale_color_manual(values = c("X" = "blue", "Y" = "red")) +
  guides(color = guide_legend(title = "Variables"))

plot(todos$forested.primary.land, todos$secondary.mean.age)
plot(todos$secondary.mean.age, todos$forested.primary.land)


plot(todos$potentially.forested.secondary.land,todos$secondary.mean.age)
plot(todos$secondary.mean.age,todos$potentially.forested.secondary.land)

##4) hacer un indice (sumando variable y utilizando una funcion condicional) que refleje la edad de la sucesión.
rasX.ForSec<- rasX.primf_2015+rasX.secdf_2015
str(rasX.ForSec)
summary(df_ForSec)
plot(df_ForSec)
df_forsec_limpio<- na.omit(df_ForSec)
datos_filtrados <- subset(df_forsec_limpio, layer != 0)


df_secma_limpio<- na.omit(df_secma)
datosfil_secma<- subset(df_secma_limpio, layer != 0)

################################################################################
#############################Hacer un condicional primer intento ##########
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.primn_2015 <-raster("states.nc", var="primn",band=1165)

rasX.secdf_2015 <-raster("states.nc", var="secdf",band=1165)
rasX.secdn_2015 <-raster("states.nc", var="secdn",band=1165)

rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)


df_primf<-as.data.frame(rasX.primf_2015)
df_secma<-as.data.frame(rasX.secma_2015)

valoresminsecma<- rasX.secma_2015==1
plot(valoresminsecma)
hist(df_secma$secondary.mean.age)

valoresmaxprimf<- rasX.primf_2015==1
plot(valoresmaxprimf)
x<-unique(df_primf)
hist(df_primf$forested.primary.land)


regla <- c(1, 1, 1098.66602)
secma_reasig <- reclassify(rasX.secma_2015, regla)

coords <- xyFromCell(rasX.secma_2015, 1:ncell(rasX.secma_2015))
valores_iguales_a_1 <- extract(rasX.secma_2015, coords[as.logical(rasX.secma_2015[] == 1), ])

# Para poder elimina los datos donde sea =1 en dos raster, maxprimf y minsecma, se debe hacer:

exclusion <- function(x, y) {
  resultado <- ifelse(x == 1 & y == 1, NA, ifelse(x == 1 | y == 1, 1, NA))
  return(resultado)
}

resultado_exclusion <- overlay(valoresmaxprimf, valoresminsecma, fun = exclusion)
resultado_exclusion<- raster(resultado_exclusion)
plot(resultado_exclusion)


#con difff
rasx<- valoresminsecma-valoresmaxprimf
plot(rasx)

Spatial(sts$primf)
library(sp)

##########################################################SEGUNDO#####
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)
rasX.primn_2015 <-raster("states.nc", var="primn",band=1165)
rasX.secdf_2015 <-raster("states.nc", var="secdf",band=1165)
rasX.secdn_2015 <-raster("states.nc", var="secdn",band=1165)

rasX.PFYNF<- rasX.primf_2015+rasX.primn_2015
rasX.SFYNF<- rasX.secdf_2015+rasX.secdn_2015

suma_PFYNF <- overlay(rasX.primf_2015, rasX.primn_2015, fun = function(x, y) {
  ifelse(x == y, x, x + y)})
# los codigos anteriores dan el mismo resultado


max.secma<- cellStats(rasX.secma_2015,stat = "max", na.rm = TRUE)
rasX.secma_2015[rasX.PFYNF == 1]<-max.secma
summary(rasX.secma_2015@data@values)

plot(rasX.secma_2015)

rasX.primf_2015[rasX.secma_2015 == 1]<-max.secma
plot(rasX.primf_2015)

###################### AUTOCORRELACION ESPACIAL###############
install.packages("spdep")
library(spdep)
library(sp)
#establecer latitudes mex
latitud_min <- 14.5
latitud_max <- 32.7
longitud_min <- -118.4
longitud_max <- -86.7

num_puntos <- 100  # Número de puntos de interés
set.seed(123)  # Para reproducibilidad
#coordenadas aleatorias
random_coords <- data.frame(
  Longitud = runif(num_puntos, longitud_min, longitud_max),
  Latitud = runif(num_puntos, latitud_min, latitud_max))

#crear sapatialpoints
puntos_interes_mexico <- SpatialPointsDataFrame(
  coords = random_coords,
  data = data.frame(Nombre = 1:num_puntos))

raster_matrix <- raster::extract(rasX.secma_2015, rasX.primf_2015, method = "simple")##nosepudo
values_secma <- extract(rasX.secma_2015, puntos_interes_mexico)
values_primf <- extract(rasX.primf_2015, puntos_interes_mexico)

distancias <- spdep::dnearneigh(coordinates(puntos_interes_mexico), d1 = 0, d2 = 50) # Ajusta el valor de d2
w <- spdep::nb2listw(distancias, style = "B")

complete_cases <- complete.cases(values_secma, values_primf)
values_secma <- values_secma[complete_cases]
values_primf <- values_primf[complete_cases]
moran_result_secma <- spdep::moran.test(values_secma, w)
moran_result_primf <- spdep::moran.test(values_primf, w)

moran<- moran.test(values_secma,w)

################otro intento mooran#############
nb <- cell2nb(primf, type = "queen")

rasX.PFYNF[is.na(rasX.PFYNF)]<-0
as.data.frame(rasX.PFYNF)

plot(rasX.PFYNF)

primf<- ncvar_get(sts, "primf")















##################12/10/23##########################ver NAs ese sera mi clasificador, y d3e4spues ese sera pesto a prueba en un clasificador (rpart)
#excluir secma y biomass

mask_na<- is.na(rasX.secma_2015)
plot(mask_na)
max.secma2<- 
 ## primero indice y despues valor booleano

quantile(rasX.secma_2015)
cut(rasX.secma_2015, quantile(rasX.secma_2015))
secma_viejo <- rasX.secma_2015[rasX.secma_2015 >= quantile(rasX.secma_2015, probs = 0.75)]
plot(secma_viejo)

cuartil_4 <- quantile(values(rasX.secma_2015), probs = 0.75, na.rm=T)
mascara_cuartil_4 <- rasX.secma_2015 > cuartil_4
plot(mascara_cuartil_4)

cuartil_1<- quantile(values(rasX.secma_2015), probs = 0, na.rm= T)
mascara_cuartil_1 <- rasX.secma_2015 >cuartil_1
plot(mascara_cuartil_1)
cuartil_1

cuartil_3<- quantile(values(rasX.secma_2015), probs =0.50, na.rm= T)
mascara_cuartil_3 <- rasX.secma_2015 > cuartil_3
plot(mascara_cuartil_3)

cuartil_2<- quantile(values(rasX.secma_2015), probs =0.25, na.rm= T)
mascara_cuartil_2<- rasX.secma_2015 >cuartil_2
plot(mascara_cuartil_2)


##ejemplo profe
X= 1:1000
nas=sample(X,10)
sample(X[is.na(match(X,nas))],length(nas))
q=as.numeric(cut(X,quantile(X), include.lowest =T))
##mio
nas<- sample(rasX.secma_2015,10000)
d<- sample(rasX.secma_2015[is.na(match(rasX.secma_2015, nas))], length(nas))
f = as.numeric(cut(rasX.secma_2015, quantile(rasX.secma_2015), include.lowest = TRUE))
##no funciona

## seleccionar el cuartil mas alto, usarlo como clasificador o usar maximos de secma


sts<-nc_open("states.nc")
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

df_primf<-as.data.frame(rasX.primf_2015)
df_primn<-as.data.frame(rasX.primn_2015)
df_secdf<-as.data.frame(rasX.secdf_2015)
df_secdn<-as.data.frame(rasX.secdn_2015)
df_secma<-as.data.frame(rasX.secma_2015)
df_urban<-as.data.frame(rasX.urban_2015)
df_c3ann<-as.data.frame(rasX.c3ann_2015)
df_c4ann<-as.data.frame(rasX.c4ann_2015)#
df_c3per<-as.data.frame(rasX.c3per_2015)
df_c4per<-as.data.frame(rasX.c4per_2015)
df_c3nfx<-as.data.frame(rasX.c3nfx_2015)
df_pastr<-as.data.frame(rasX.pastr_2015)
df_range<-as.data.frame(rasX.range_2015)
df_secmb<-as.data.frame(rasX.secmb_2015)
df_PFYNF<- as.data.frame(rasX.PFYNF)
df_mascara_4<-as.data.frame(mascara_cuartil_4)

df_states_rasters<- data.frame(df_primf,df_primn,df_secdf,df_secdn,df_secma,df_urban,
                               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,df_secmb,
                               df_PFYNF, df_mascara_4)##<- transformar a un special object de sp

library(rpart.plot)
library (rpart)
modelo_cuartil_4<- rpart(layer.1~
                 + potentially.forested.secondary.land+
                 potentially.non.forested.secondary.land+
                 C3.annual.crops+ C4.annual.crops+ C4.perennial.crops+
                 C3.perennial.crops+C3.nitrogen.fixing.crops+
                 managed.pasture+ rangeland+
                 C3.annual.crops+ urban.land+ forested.primary.land+ non.forested.primary.land
               ,data = df_states_rasters, method = "class")

rpart.plot(modelo)

## comparar con el mismo secma, clase (cuartil 4), no clase (cuaril 1,2,3) 16/10/2023

df_cuartil1 <- as.data.frame(mascara_cuartil_1)
df_cuartil2 <- as.data.frame(mascara_cuartil_2)
df_cuartil3 <- as.data.frame(mascara_cuartil_3)
df_cuartil4 <- as.data.frame(mascara_cuartil_4)

df_cuartiles<- data.frame(df_cuartil1, df_cuartil2, df_cuartil3, df_cuartil4)

modelo_cuartil<- rpart(layer.3~ 
                 + layer.2 + layer + layer.1
               ,data = df_cuartiles, method = "class")
rpart.plot(modelo_cuartil)

## hacer otro sobre el primer cuartil (modelo2)

df_states_rasters<- data.frame(df_primf,df_primn,df_secdf,df_secdn,df_secma,df_urban,
                               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,df_secmb,
                               df_PFYNF, df_mascara_4)##<- transformar a un special object de sp

library(rpart.plot)
library (rpart)
modelo_cuartil_4<- rpart(layer.1~
                           + potentially.forested.secondary.land+
                           potentially.non.forested.secondary.land+
                           C3.annual.crops+ C4.annual.crops+ C4.perennial.crops+
                           C3.perennial.crops+C3.nitrogen.fixing.crops+
                           managed.pasture+ rangeland+
                           C3.annual.crops+ urban.land+ forested.primary.land+ non.forested.primary.land
                         ,data = df_states_rasters, method = "class")

rpart.plot(modelo)

################### FALLIDO asirgnar el grupo del cuartil con numero 1,2,3,4 a los datos contenidos en mascara

divided_rasters <- disaggregate(rasX.secma_2015, fact = 2)

# Crear una lista para almacenar los cuartiles de cada parte
quartile_rasters <- list()

calculate_quartiles <- function(x) {
  # Calcular los cuartiles
  quartiles <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Asignar cuartiles
  cuartiles <- cut(x, breaks = quartiles, labels = 1:4)
  
  return(cuartiles)
}

# Calcular los cuartiles para cada parte del raster
for (i in 1:4) {
  # Extraer valores de cada parte
  values_part <- extract(divided_rasters[[i]], divided_rasters[[i]])
  
  # Calcular los cuartiles y asignarlos a la parte del raster
  quartile_rasters[[i]] <- calculate_quartiles(values_part)
}

############# FALLIDO INTENTO 2

# Obtener la extensión del raster
ext <- extent(rasX.secma_2015)

# Calcular los límites de las 4 partes
xmid <- (ext@xmin + ext@xmax) / 2
ymid <- (ext@ymin + ext@ymax) / 2

# Crear las extensiones para cada parte
part1 <- extent(ext@xmin, xmid, ext@ymin, ymid)
part2 <- extent(xmid, ext@xmax, ext@ymin, ymid)
part3 <- extent(ext@xmin, xmid, ymid, ext@ymax)
part4 <- extent(xmid, ext@xmax, ymid, ext@ymax)

# Recortar el raster en 4 partes iguales
part_raster1 <- crop(rasX.secma_2015, part1)
part_raster2 <- crop(rasX.secma_2015, part2)
part_raster3 <- crop(rasX.secma_2015, part3)
part_raster4 <- crop(rasX.secma_2015, part4)

#### otra manera
raster_dividido <- cut(getValues(rasX.secma_2015), breaks = 4, labels = 1:4)
raster_dividido <- setValues(rasX.secma_2015, raster_dividido)
plot(raster_dividido)
df_dividido <- as.data.frame(raster_dividido)










