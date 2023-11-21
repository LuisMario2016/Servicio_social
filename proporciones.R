setwd("C:/serviciosocial/DATOS")
library(raster)     # package for raster manipulation
library (sp)        # package for spatial objects
library(ncdf4)      # package for netcdf manipulation
library(sf)
install.packages("rgdal")
library(rgdal)
library(ggplot2)
library(readr)
################################# Para primf ###################################
rasX.primf_2015 <-raster("states.nc", var="primf",band=1165)
rasX.primf_2000 <-raster("states.nc", var="primf",band=1150)
rasX.primf_1985 <-raster("states.nc", var="primf",band=1135)
rasX.primf_1970 <-raster("states.nc", var="primf",band=1120)
rasX.primf_1955 <-raster("states.nc", var="primf",band=1105)
rasX.primf_1940 <-raster("states.nc", var="primf",band=1090)
rasX.primf_1925 <-raster("states.nc", var="primf",band=1075)
rasX.primf_1910 <-raster("states.nc", var="primf",band=1060)
rasX.primf_1895 <-raster("states.nc", var="primf",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)
values_by_country <- extract(rasX.primf_1895, land) ##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
primf_prop1895<- as.data.frame(proportion_country)

rm(values_by_country)

primf<- cbind.data.frame(primf_prop1895,primf_prop1910,primf_prop1925,primf_prop1940,
                          primf_prop1955,primf_prop1970, primf_prop1985, primf_prop2000,
                          primf_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(primf) <- years

ggplot(primf_prop1895, aes(x = rownames(primf_prop1895), y = primf_prop1895$proportion_country)) +
  geom_point() +
  ylim(0, 1) +  # Establecer el límite en el eje y
  ggtitle("Gráfico de dispersión con límite en el eje y")


ruta<- "C:\\serviciosocial"
write.csv(primf, file.path(ruta, "primf.csv"), row.names =TRUE)
write.table(primf, file = file.path(ruta, "primf"), sep = ",", col.names = TRUE, row.names = FALSE)
############################### para primn #####################################
rasX.primn_2015 <-raster("states.nc", var="primn",band=1165)
rasX.primn_2000 <-raster("states.nc", var="primn",band=1150)
rasX.primn_1985 <-raster("states.nc", var="primn",band=1135)
rasX.primn_1970 <-raster("states.nc", var="primn",band=1120)
rasX.primn_1955 <-raster("states.nc", var="primn",band=1105)
rasX.primn_1940 <-raster("states.nc", var="primn",band=1090)
rasX.primn_1925 <-raster("states.nc", var="primn",band=1075)
rasX.primn_1910 <-raster("states.nc", var="primn",band=1060)
rasX.primn_1895 <-raster("states.nc", var="primn",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.primn_1895, land)

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
primn_prop1895<- as.data.frame(proportion_country)

rm(values_by_country)

primn<- cbind.data.frame(primn_prop1895,primn_prop1910,primn_prop1925,primn_prop1940,
                         primn_prop1955,primn_prop1970, primn_prop1985, primn_prop2000,
                         primn_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(primn) <- years

ruta<- "C:\\serviciosocial"
write.csv(primn, file.path(ruta, "primn.csv"), row.names =TRUE)
############################# para secdf #######################################
rasX.secdf_2015 <-raster("states.nc", var="secdf",band=1165)
rasX.secdf_2000 <-raster("states.nc", var="secdf",band=1150)
rasX.secdf_1985 <-raster("states.nc", var="secdf",band=1135)
rasX.secdf_1970 <-raster("states.nc", var="secdf",band=1120)
rasX.secdf_1955 <-raster("states.nc", var="secdf",band=1105)
rasX.secdf_1940 <-raster("states.nc", var="secdf",band=1090)
rasX.secdf_1925 <-raster("states.nc", var="secdf",band=1075)
rasX.secdf_1910 <-raster("states.nc", var="secdf",band=1060)
rasX.secdf_1895 <-raster("states.nc", var="secdf",band=1045)

land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
country<-as.list(land$ADMIN)

values_by_country <- extract(rasX.secdf_1895, land) ##

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
secdf_prop1895<- as.data.frame(proportion_country)##

rm(values_by_country)

secdf<- cbind.data.frame(secdf_prop1985,secdf_prop1910,secdf_prop1925,secdf_prop1940,
                         secdf_prop1955,secdf_prop1970, secdf_prop1985, secdf_prop2000,
                         secdf_prop2015)
years <- seq(1895, 2015, by = 15)
colnames(secdf) <- years

ruta<- "C:\\serviciosocial"
write.csv(secdf, file.path(ruta, "secdf.csv"), row.names =TRUE)

############################# para urban #######################################
rasX.urban_2015 <-raster("states.nc", var="urban",band=1165)







############################## c3ann2015 #######################################
rasX.c3ann_2015 <-raster("states.nc", var="c3ann",band=1165)
rasX.c4ann_2015 <-raster("states.nc", var="c4ann",band=1165)
rasX.c3per_2015 <-raster("states.nc", var="c3per",band=1165)
rasX.c4per_2015 <-raster("states.nc", var="c4per",band=1165)
rasX.c3nfx_2015 <-raster("states.nc", var="c3nfx",band=1165)
rasX.pastr_2015 <-raster("states.nc", var="pastr",band=1165)
rasX.range_2015 <-raster("states.nc", var="range",band=1165)
rasX.secmb_2015 <-raster("states.nc", var="secmb",band=1165)
rasX.secma_2015 <-raster("states.nc", var="secma",band=1165)
