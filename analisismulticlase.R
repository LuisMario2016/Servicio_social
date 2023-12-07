setwd("C:/serviciosocial/DATOS")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(sf) # package for geospatial analysis
library(ggplot2)

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

rasX.FYNF_2015<- rasX.primf_2015+rasX.primn_2015
rasX.SFYNF<- rasX.secdf_2015+rasX.secdn_2015

max.secma<- cellStats(rasX.secma_2015,stat = "max", na.rm = TRUE)
rasX.secma_2015[rasX.FYNF_2015 == 1]<-max.secma
summary(rasX.secma_2015@data@values)

plot(rasX.secma_2015)

rasX.primf_2015[rasX.secma_2015 == 1]<-max.secma

cuartil_4 <- quantile(values(rasX.secma_2015), probs = 0.75, na.rm=T)
mascara_cuartil_4 <- rasX.secma_2015 > cuartil_4

cuartil_1<- quantile(values(rasX.secma_2015), probs = 0, na.rm= T)
mascara_cuartil_1 <- rasX.secma_2015 >cuartil_1

cuartil_3<- quantile(values(rasX.secma_2015), probs =0.50, na.rm= T)
mascara_cuartil_3 <- rasX.secma_2015 > cuartil_3

cuartil_2<- quantile(values(rasX.secma_2015), probs =0.25, na.rm= T)
mascara_cuartil_2<- rasX.secma_2015 >cuartil_2


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
df_mascara_4<-as.data.frame(mascara_cuartil_4)
df_mascara_3<-as.data.frame(mascara_cuartil_3)
df_mascara_2<-as.data.frame(mascara_cuartil_2)
df_mascara_1<-as.data.frame(mascara_cuartil_1)

df_states_rasters<- data.frame(df_primf,df_primn,df_secdf,df_secdn,df_secma,df_urban,
                               df_c3ann, df_c4ann, df_c4per, df_c3per, df_c3nfx,df_pastr,df_range,df_secmb,
                               df_FYNF, df_mascara_4,df_mascara_3
                               ,df_mascara_2,df_mascara_1)

library(rpart.plot)
library (rpart)
df_states_rasters$layer.1 <- as.factor(df_states_rasters$layer.1)
df_states_rasters$layer.2 <- as.factor(df_states_rasters$layer.2)
df_states_rasters$layer.3 <- as.factor(df_states_rasters$layer.3)
df_states_rasters$layer.4 <- as.factor(df_states_rasters$layer.4)
df_states_rasters$combined_response <- interaction(df_states_rasters$layer.1, 
                                                   df_states_rasters$layer.2, 
                                                   df_states_rasters$layer.3, 
                                                   df_states_rasters$layer.4)

modelo_multicuartil <- rpart(combined_response ~ 
                               potentially.forested.secondary.land + 
                               potentially.non.forested.secondary.land + 
                               C3.annual.crops + C4.annual.crops + 
                               C4.perennial.crops + C3.perennial.crops + 
                               C3.nitrogen.fixing.crops + managed.pasture + 
                               rangeland + C3.annual.crops + urban.land + 
                               forested.primary.land + non.forested.primary.land,
                             data = df_states_rasters, method = "class")
rpart.plot(modelo_multicuartil, extra = 0, cex = .6, uniform = TRUE, space=0.0000000000000001)



