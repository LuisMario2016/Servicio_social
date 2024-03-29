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
                               df_FYNF)
df_states<- na.exclude(df_states_rasters)
y= cut(df_states$secondary.mean.age,quantile(df_states$secondary.mean.age), include.lowest =T)
y<-as.integer(y)
df_states$cuartiles=y
                       
library(rpart.plot)
library (rpart)

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



















