setwd("C:/serviciosocial/DATOS")
library(readxl)
paisesmad_SS <- read_excel("paisesmad_SS.xlsx", 
                             sheet = "GDP pc")
View(paisesmad_SS)

years_interes <- seq(1895, 2015, by = 15)
PIB<- as.data.frame(paisesmad_SS)

install.packages("tidyr")
library(tidyr)
library(dplyr)
Filtro <- PIB %>% 
  filter(`GDP pc 2011 prices` %in% years_interes)

PIB_transpuesto <- t(Filtro)
colnames(PIB_transpuesto) <- unlist(PIB_transpuesto[1, ])
PIBTrans<- PIB_transpuesto[-1,]
PIBTrans <- PIBTrans[rowSums(is.na(PIBTrans)) <= 2, ]
PIB<-as.data.frame(PIBTrans)
ruta<- "C:\\serviciosocial"
write.csv(PIB, file.path(ruta, "PIB.csv"))
###################
paisesmad_SS <- read_excel("paisesmad_SS.xlsx", 
                           sheet = "Population")
POP<- as.data.frame(paisesmad_SS)
FiltroPOP <- POP %>% 
  filter(Population%in% years_interes)

POPTrans<- t(FiltroPOP)
colnames(POPTrans) <- unlist(POPTrans[1, ])
POPTrans<- POPTrans[-1,]
POPTrans <- POPTrans[rowSums(is.na(POPTrans)) <= 2, ]
POP<-as.data.frame(POPTrans)
ruta<- "C:\\serviciosocial"
write.csv(POP, file.path(ruta, "POP.csv"))
######################### eligiendo paises#########
PIB <- read.csv("C:/serviciosocial/PIB.csv")
POP <- read.csv("C:/serviciosocial/POP.csv")
resultado <- merge(POP, PIB, by = "X")# x es para pop y "y" para pib
top_filas <- resultado %>%
  slice_max(order_by = everything(), n = 10)
######################### EXtrayendo indices ##################################
######################## Mexico ################################################
#importar todos los cvs con datos de registro y extraer pais por pais para graf
setwd("C:/serviciosocial/DATOS")
library(readxl)

primn <- read.csv("C:/serviciosocial/proporciones/primn.csv")
primf <- read.csv("C:/serviciosocial/proporciones/primf.csv")
range <- read.csv("C:/serviciosocial/proporciones/urban.csv")
urban <- read.csv("C:/serviciosocial/proporciones/range.csv")
secdf <- read.csv("C:/serviciosocial/proporciones/secdf.csv")
secdn <- read.csv("C:/serviciosocial/proporciones/secdn.csv")
#Urban <- read.csv("C:/serviciosocial/proporciones/urban.csv")
#Urban <- read.csv("C:/serviciosocial/proporciones/urban.csv")
#Urban <- read.csv("C:/serviciosocial/proporciones/urban.csv")
secma <- read.csv("C:/serviciosocial/proporciones/secma.csv")
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "Mexico")
})
Mexico <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(Mexico)[1:3] <- nuevos_nombres
Mexico<-Mexico[,-1]
Mexico<-t(Mexico)
ruta<- "C:\\serviciosocial"
write.csv(Mexico, file.path(ruta, "Mexico.csv")) ##

plotmex <- read.csv("C:/serviciosocial/Mexico.csv")

matplot(Prueba$X, Prueba[, -c(1,5)], type = "l", col = c("red", "blue", "green"),
        lty = 1, lwd = 2, xlab = "Año", ylab = "Valor",
        main = "Gráfica de Líneas para Tres Variables")

plot(plotmex$X, plotmex$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotmex$X, plotmex$primf, col = "red")
points(plotmex$X, plotmex$urban, col = "")
points(plotmex$X, plotmex$range, col = "")
points(plotmex$X, plotmex$secdf, col = "")
points(plotmex$X, plotmex$secdn, col = "")
points(plotmex$X, plotmex$, col = "")
points(plotmex$X, plotmex$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
################### UNITED KINGDOM #############################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "UNITED KINGDOM")})

UK <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(UK)[1:3] <- nuevos_nombres
UK<-UK[,-1]
UK<-t(UK)
ruta<- "C:\\serviciosocial"
write.csv(UK, file.path(ruta, "UK.csv")) ##

plotUK <- read.csv("C:/serviciosocial/UK.csv")

matplot(Prueba$X, Prueba[, -c(1,5)], type = "l", col = c("red", "blue", "green"),
        lty = 1, lwd = 2, xlab = "Año", ylab = "Valor",
        main = "Gráfica de Líneas para Tres Variables")

plot(plotUK$X, plotUK$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotUK$X, plotUK$primf, col = "red")
points(plotUK$X, plotUK$urban, col = "")
points(plotUK$X, plotUK$range, col = "")
points(plotUK$X, plotUK$secdf, col = "")
points(plotUK$X, plotUK$secdn, col = "")
points(plotUK$X, plotUK$, col = "")
points(plotUK$X, plotUK$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
############## Australia #######################################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "Australia")})

AUS <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(AUS)[1:3] <- nuevos_nombres
AUS<-AUS[,-1]
AUS<-t(AUS)
ruta<- "C:\\serviciosocial"
write.csv(AUS, file.path(ruta, "AUS.csv")) ##

plotAUS <- read.csv("C:/serviciosocial/AUS.csv")

matplot(Prueba$X, Prueba[, -c(1,5)], type = "l", col = c("red", "blue", "green"),
        lty = 1, lwd = 2, xlab = "Año", ylab = "Valor",
        main = "Gráfica de Líneas para Tres Variables")

plot(plotAUS$X, plotAUS$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotAUS$X, plotAUS$primf, col = "red")
points(plotAUS$X, plotAUS$urban, col = "")
points(plotAUS$X, plotAUS$range, col = "")
points(plotAUS$X, plotAUS$secdf, col = "")
points(plotAUS$X, plotAUSddlkkn $secdn, col = "")
points(plotAUS$X, plotAUS$, col = "")
points(plotAUS$X, plotAUS$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
############## EUA #######################################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "United States")})

EUA <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(EUA)[1:3] <- nuevos_nombres
EUA<-EUA[,-1]
EUA<-t(EUA)
ruta<- "C:\\serviciosocial"
write.csv(EUA, file.path(ruta, "EUA.csv")) ##

plotEUA <- read.csv("C:/serviciosocial/EUA.csv")

plot(plotEUA$X, plotEUA$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotEUA$X, plotEUA$primf, col = "red")
points(plotEUA$X, plotEUA$urban, col = "")
points(plotEUA$X, plotEUA$range, col = "")
points(plotEUA$X, plotEUA$secdf, col = "")
points(plotEUA$X, plotEUA$secdn, col = "")
points(plotEUA$X, plotEUA$, col = "")
points(plotEUA$X, plotEUA$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
############## INDIA #######################################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "INDIA")})

IND <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(IND)[1:3] <- nuevos_nombres
IND<-IND[,-1]
IND<-t(IND)
ruta<- "C:\\serviciosocial"
write.csv(IND, file.path(ruta, "INDIA.csv")) ##

plotIND <- read.csv("C:/serviciosocial/INDIA.csv")

plot(plotIND$X, plotIND$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotIND$X, plotIND$primf, col = "red")
points(plotIND$X, plotIND$urban, col = "")
points(plotIND$X, plotIND$range, col = "")
points(plotIND$X, plotIND$secdf, col = "")
points(plotIND$X, plotIND$secdn, col = "")
points(plotIND$X, plotIND$, col = "")
points(plotIND$X, plotIND$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
############## BRAZIL    #######################################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "Bazil")})

BRA <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(BRA)[1:3] <- nuevos_nombres
BRA<-BRA[,-1]
BRA<-t(BRA)
ruta<- "C:\\serviciosocial"
write.csv(BRA, file.path(ruta, "BRAZIL.csv")) ##

plotBRA <- read.csv("C:/serviciosocial/BRAZIL.csv")

plot(plotBRA$X, plotBRA$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotBRA$X, plotBRA$primf, col = "red")
points(plotBRA$X, plotBRA$urban, col = "")
points(plotBRA$X, plotBRA$range, col = "")
points(plotBRA$X, plotBRA$secdf, col = "")
points(plotBRA$X, plotBRA$secdn, col = "")
points(plotBRA$X, plotBRA$, col = "")
points(plotBRA$X, plotBRA$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))
############## France    #######################################################
lista<-list(primf,primn,urban,range,secdf,secdn)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "France")})

FRA <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban","range", "secdf", "secdn")
row.names(FRA) <- nuevos_nombres
FRA<-FRA[,-1]
FRA<-t(FRA)
ruta<- "C:\\serviciosocial"
write.csv(FRA, file.path(ruta, "FRA.csv")) ##

plotFRA <- read.csv("C:/serviciosocial/BRAZIL.csv")

plot(plotFRA$X, plotFRA$primn, ylim = c(0,1), type = "l", col = "blue")
points(plotFRA$X, plotFRA$primf, col = "red")
points(plotFRA$X, plotFRA$urban, col = "")
points(plotFRA$X, plotFRA$range, col = "")
points(plotFRA$X, plotFRA$secdf, col = "")
points(plotFRA$X, plotFRA$secdn, col = "")
points(plotFRA$X, plotFRA$, col = "")
points(plotFRA$X, plotFRA$, col = "")
legend("topright", legend = c("primn", "primf","urban","range
                              ", "secdf","secdn",""), col = c("blue", "red"), lty = 1, pch = c(1, 2))





