setwd("C:/serviciosocial/DATOS")
library(raster)     # package for raster manipulation
library (sp)        # package for spatial objects
library(ncdf4)      # package for netcdf manipulation
library(sf)
install.packages("rgdal")
library(rgdal)

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


map <- st_read("C:/serviciosocial/DATOS/ne_10m_admin_0_countries.shp")
values_by_country <- extract(rasX.primf_2015, mapa)
values_by_country <- raster::extract(rasX.primf_2015, mapa)

summary(values_by_country)
land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
values_by_country <- extract(rasX.primf_2015, land)

country<-as.list(land$ADMIN)

for (i in 1:length(country)) {
  names(values_by_country)[i] <- country[i]
}
summary(values_by_country)
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
View(proportion_country)

#################################################### Con centroides
centroids <- rasterToPoints(rasX.primf_2015)
centroids <- SpatialPointsDataFrame(coords = centroids, data = data.frame(ID = 1:nrow(centroids)))
projection(centroids) <- projection(rasX.primf_2015)

values_country_centroid<- extract(centroids, land)

centroidsWithCountries <- sp::over(centroids, land)

centroids_sf <- st_as_sf(centroids)
land_sf <- st_as_sf(land)
centroids_with_land <- st_join(land_sf, centroids_sf, join = st_intersects)
land_sf <- st_make_valid(land_sf)
centroids_sf <- st_make_valid(centroids_sf)
indices_poligonos <- st_within(centroids_sf, land_sf)
valores_poligonos <- rasX.primf_2015$forested.primary.land[indices_poligonos]





