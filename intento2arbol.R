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

rasX.PFYNF<- rasX.primf_2015+rasX.primn_2015
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





