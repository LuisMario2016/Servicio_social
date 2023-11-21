setwd("C:/serviciosocial/DATOS")
library(raster)     # package for raster manipulation
library (sp)        # package for spatial objects
library(ncdf4)      # package for netcdf manipulation
library(sf)
library(rgdal)
library(ggplot2)
library(readr)
##############################para secdn #######################################
rasX.secdn_2015 <-raster("states.nc", var="secdn",band=1165)
rasX.secdn_2000 <-raster("states.nc", var="secdn",band=1150)
rasX.secdn_1985 <-raster("states.nc", var="secdn",band=1135)
rasX.secdn_1970 <-raster("states.nc", var="secdn",band=1120)
rasX.secdn_1955 <-raster("states.nc", var="secdn",band=1105)
rasX.secdn_1940 <-raster("states.nc", var="secdn",band=1090)
rasX.secdn_1925 <-raster("states.nc", var="secdn",band=1075)
rasX.secdn_1910 <-raster("states.nc", var="secdn",band=1060)
rasX.secdn_1895 <-raster("states.nc", var="secdn",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.secdn_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
secdn_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

secdn<- cbind.data.frame(secdn_prop1895,secdn_prop1910,secdn_prop1925,secdn_prop1940,
                         secdn_prop1955,secdn_prop1970, secdn_prop1985, secdn_prop2000,
                         secdn_prop2015)
secdn <- secdn[, order(names(secdn))]
years <- seq(1895, 2015, by = 15)
colnames(secdn) <- years

ruta<- "C:\\serviciosocial"
write.csv(secdn, file.path(ruta, "secdn.csv"), row.names =TRUE) ##

############################# para urban #######################################
rasX.urban_2015 <-raster("states.nc", var="urban",band=1165)
rasX.urban_2000 <-raster("states.nc", var="urban",band=1150)
rasX.urban_1985 <-raster("states.nc", var="urban",band=1135)
rasX.urban_1970 <-raster("states.nc", var="urban",band=1120)
rasX.urban_1955 <-raster("states.nc", var="urban",band=1105)
rasX.urban_1940 <-raster("states.nc", var="urban",band=1090)
rasX.urban_1925 <-raster("states.nc", var="urban",band=1075)
rasX.urban_1910 <-raster("states.nc", var="urban",band=1060)
rasX.urban_1895 <-raster("states.nc", var="urban",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.urban_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
urban_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

urban<- cbind.data.frame(urban_prop1895,urban_prop1910,urban_prop1925,urban_prop1940,
                         urban_prop1955,urban_prop1970, urban_prop1985, urban_prop2000,
                         urban_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(urban) <- years

ruta<- "C:\\serviciosocial"
write.csv(urban, file.path(ruta, "urban.csv"), row.names =TRUE) ##
############################## c3ann2015 #######################################
rasX.c3ann_2015 <-raster("states.nc", var="c3ann",band=1165)
rasX.c3ann_2000 <-raster("states.nc", var="c3ann",band=1150)
rasX.c3ann_1985 <-raster("states.nc", var="c3ann",band=1135)
rasX.c3ann_1970 <-raster("states.nc", var="c3ann",band=1120)
rasX.c3ann_1955 <-raster("states.nc", var="c3ann",band=1105)
rasX.c3ann_1940 <-raster("states.nc", var="c3ann",band=1090)
rasX.c3ann_1925 <-raster("states.nc", var="c3ann",band=1075)
rasX.c3ann_1910 <-raster("states.nc", var="c3ann",band=1060)
rasX.c3ann_1895 <-raster("states.nc", var="c3ann",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.c3ann_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
c3ann_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

c3ann<- cbind.data.frame(c3ann_prop1895,c3ann_prop1910,c3ann_prop1925,c3ann_prop1940,
                         c3ann_prop1955,c3ann_prop1970, c3ann_prop1985, c3ann_prop2000,
                         c3ann_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(c3ann) <- years

ruta<- "C:\\serviciosocial"
write.csv(c3ann, file.path(ruta, "c3ann.csv"), row.names =TRUE) ##
##############################c4ann ############################################
rasX.c4ann_2015 <-raster("states.nc", var="c4ann",band=1165)
rasX.c3per_2015 <-raster("states.nc", var="c3per",band=1165)
rasX.c4per_2015 <-raster("states.nc", var="c4per",band=1165)
rasX.c3nfx_2015 <-raster("states.nc", var="c3nfx",band=1165)
rasX.pastr_2015 <-raster("states.nc", var="pastr",band=1165)
############################# range ############################################
rasX.range_2015 <-raster("states.nc", var="range",band=1165)
rasX.range_2000 <-raster("states.nc", var="range",band=1150)
rasX.range_1985 <-raster("states.nc", var="range",band=1135)
rasX.range_1970 <-raster("states.nc", var="range",band=1120)
rasX.range_1955 <-raster("states.nc", var="range",band=1105)
rasX.range_1940 <-raster("states.nc", var="range",band=1090)
rasX.range_1925 <-raster("states.nc", var="range",band=1075)
rasX.range_1910 <-raster("states.nc", var="range",band=1060)
rasX.range_1895 <-raster("states.nc", var="range",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.range_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
range_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

range<- cbind.data.frame(range_prop1895,range_prop1910,range_prop1925,range_prop1940,
                         range_prop1955,range_prop1970,range_prop1985, range_prop2000,
                         range_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(range) <- years

ruta<- "C:\\serviciosocial"
write.csv(range, file.path(ruta, "range.csv"), row.names =TRUE) ##
############################ secmb #############################################
rasX.secmb_2015 <-raster("states.nc", var="secmb",band=1165)
rasX.secmb_2000 <-raster("states.nc", var="secmb",band=1150)
rasX.secmb_1985 <-raster("states.nc", var="secmb",band=1135)
rasX.secmb_1970 <-raster("states.nc", var="secmb",band=1120)
rasX.secmb_1955 <-raster("states.nc", var="secmb",band=1105)
rasX.secmb_1940 <-raster("states.nc", var="secmb",band=1090)
rasX.secmb_1925 <-raster("states.nc", var="secmb",band=1075)
rasX.secmb_1910 <-raster("states.nc", var="secmb",band=1060)
rasX.secmb_1895 <-raster("states.nc", var="secmb",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.secmb_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
secmb_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

secmb<- cbind.data.frame(secmb_prop1895,secmb_prop1910,secmb_prop1925,secmb_prop1940,
                         secmb_prop1955,secmb_prop1970, secmb_prop1985, secmb_prop2000,
                         secmb_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(secmb) <- years

ruta<- "C:\\serviciosocial"
write.csv(secmb, file.path(ruta, "secmb.csv"), row.names =TRUE) ##
########################### secma ##############################################
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)
rasX.secma_2000 <-raster("states.nc", var="secma",band=1150)
rasX.secma_1985 <-raster("states.nc", var="secma",band=1135)
rasX.secma_1970 <-raster("states.nc", var="secma",band=1120)
rasX.secma_1955 <-raster("states.nc", var="secma",band=1105)
rasX.secma_1940 <-raster("states.nc", var="secma",band=1090)
rasX.secma_1925 <-raster("states.nc", var="secma",band=1075)
rasX.secma_1910 <-raster("states.nc", var="secma",band=1060)
rasX.secma_1895 <-raster("states.nc", var="secma",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.secma_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
secma_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

secma<- cbind.data.frame(secma_prop1895,secma_prop1910,secma_prop1925,secma_prop1940,
                         secma_prop1955,secma_prop1970, secma_prop1985, secma_prop2000,
                         secma_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(secma) <- years

ruta<- "C:\\serviciosocial"
write.csv(secma, file.path(ruta, "secma.csv"), row.names =TRUE) ##


