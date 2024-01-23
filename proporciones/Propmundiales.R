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

values_by_country <- extract(rasX.secdn_1895,land) ##Se cambia de forma manual la variable del año 

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
secdn_prop1895<- as.data.frame(proportion_country) ##Se ajusta de acuerdo al año

rm(values_by_country)

secdn<- cbind.data.frame(secdn_prop1895,secdn_prop1910,secdn_prop1925,secdn_prop1940,
                         secdn_prop1955,secdn_prop1970, secdn_prop1985, secdn_prop2000,
                         secdn_prop2015)
secdn <- secdn[, order(names(secdn))]
years <- seq(1895, 2015, by = 15)
colnames(secdn) <- years

ruta<- "C:\\serviciosocial"
write.csv(secdn, file.path(ruta, "secdn.csv"), row.names =TRUE)

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
rasX.c4ann_2000 <-raster("states.nc", var="c4ann",band=1150)
rasX.c4ann_1985 <-raster("states.nc", var="c4ann",band=1135)
rasX.c4ann_1970 <-raster("states.nc", var="c4ann",band=1120)
rasX.c4ann_1955 <-raster("states.nc", var="c4ann",band=1105)
rasX.c4ann_1940 <-raster("states.nc", var="c4ann",band=1090)
rasX.c4ann_1925 <-raster("states.nc", var="c4ann",band=1075)
rasX.c4ann_1910 <-raster("states.nc", var="c4ann",band=1060)
rasX.c4ann_1895 <-raster("states.nc", var="c4ann",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.c4ann_1955,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
c4ann_prop1955<- as.data.frame(proportion_country)##

rm(values_by_country)

c4ann<- cbind.data.frame(c4ann_prop1895,c4ann_prop1910,c4ann_prop1925,c4ann_prop1940,
                         c4ann_prop1955,c4ann_prop1970, c4ann_prop1985, c4ann_prop2000,
                         c4ann_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(c4ann) <- years

ruta<- "C:\\serviciosocial"
write.csv(c4ann, file.path(ruta, "c4ann.csv"), row.names =TRUE)
####################### c3per ##################################################
rasX.c3per_2015 <-raster("states.nc", var="c3per",band=1165)
rasX.c3per_2000 <-raster("states.nc", var="c3per",band=1150)
rasX.c3per_1985 <-raster("states.nc", var="c3per",band=1135)
rasX.c3per_1970 <-raster("states.nc", var="c3per",band=1120)
rasX.c3per_1955 <-raster("states.nc", var="c3per",band=1105)
rasX.c3per_1940 <-raster("states.nc", var="c3per",band=1090)
rasX.c3per_1925 <-raster("states.nc", var="c3per",band=1075)
rasX.c3per_1910 <-raster("states.nc", var="c3per",band=1060)
rasX.c3per_1895 <-raster("states.nc", var="c3per",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.c3per_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
c3per_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

c3per<- cbind.data.frame(c3per_prop1895,c3per_prop1910,c3per_prop1925,c3per_prop1940,
                         c3per_prop1955,c3per_prop1970, c3per_prop1985, c3per_prop2000,
                         c3per_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(c3per) <- years

ruta<- "C:\\serviciosocial"
write.csv(c3per, file.path(ruta, "c3per.csv"), row.names =TRUE)

####################### c4per ##################################################
rasX.c4per_2015 <-raster("states.nc", var="c4per",band=1165)
rasX.c4per_2000 <-raster("states.nc", var="c4per",band=1150)
rasX.c4per_1985 <-raster("states.nc", var="c4per",band=1135)
rasX.c4per_1970 <-raster("states.nc", var="c4per",band=1120)
rasX.c4per_1955 <-raster("states.nc", var="c4per",band=1105)
rasX.c4per_1940 <-raster("states.nc", var="c4per",band=1090)
rasX.c4per_1925 <-raster("states.nc", var="c4per",band=1075)
rasX.c4per_1910 <-raster("states.nc", var="c4per",band=1060)
rasX.c4per_1895 <-raster("states.nc", var="c4per",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.c4per_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
c4per_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

c4per<- cbind.data.frame(c4per_prop1895,c4per_prop1910,c4per_prop1925,c4per_prop1940,
                         c4per_prop1955,c4per_prop1970,c4per_prop1985,c4per_prop2000,
                         c4per_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(c4per) <- years

ruta<- "C:\\serviciosocial"
write.csv(c4per, file.path(ruta, "c4per.csv"), row.names =TRUE)
###################### c3enfx ##################################################
rasX.c3nfx_2015 <-raster("states.nc", var="c3nfx",band=1165)
rasX.c3nfx_2000 <-raster("states.nc", var="c3nfx",band=1150)
rasX.c3nfx_1985 <-raster("states.nc", var="c3nfx",band=1135)
rasX.c3nfx_1970 <-raster("states.nc", var="c3nfx",band=1120)
rasX.c3nfx_1955 <-raster("states.nc", var="c3nfx",band=1105)
rasX.c3nfx_1940 <-raster("states.nc", var="c3nfx",band=1090)
rasX.c3nfx_1925 <-raster("states.nc", var="c3nfx",band=1075)
rasX.c3nfx_1910 <-raster("states.nc", var="c3nfx",band=1060)
rasX.c3nfx_1895 <-raster("states.nc", var="c3nfx",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.c3nfx_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
c3nfx_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

c3nfx<- cbind.data.frame(c3nfx_prop1895,c3nfx_prop1910,c3nfx_prop1925,c3nfx_prop1940,
                         c3nfx_prop1955,c3nfx_prop1970,c3nfx_prop1985,c3nfx_prop2000,
                         c3nfx_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(c3nfx) <- years

ruta<- "C:\\serviciosocial"
write.csv(c3nfx, file.path(ruta, "c3nfx.csv"), row.names =TRUE)
########################## pastr ###############################################
rasX.pastr_2015 <-raster("states.nc", var="pastr",band=1165)
rasX.pastr_2000 <-raster("states.nc", var="pastr",band=1150)
rasX.pastr_1985 <-raster("states.nc", var="pastr",band=1135)
rasX.pastr_1970 <-raster("states.nc", var="pastr",band=1120)
rasX.pastr_1955 <-raster("states.nc", var="pastr",band=1105)
rasX.pastr_1940 <-raster("states.nc", var="pastr",band=1090)
rasX.pastr_1925 <-raster("states.nc", var="pastr",band=1075)
rasX.pastr_1910 <-raster("states.nc", var="pastr",band=1060)
rasX.pastr_1895 <-raster("states.nc", var="pastr",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.pastr_1895,land)##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
pastr_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

pastr<- cbind.data.frame(pastr_prop1895,pastr_prop1910,pastr_prop1925,pastr_prop1940,
                         pastr_prop1955,pastr_prop1970,pastr_prop1985,pastr_prop2000,
                         pastr_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(pastr) <- years

ruta<- "C:\\serviciosocial"
write.csv(pastr, file.path(ruta, "pastr.csv"), row.names =TRUE)
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


