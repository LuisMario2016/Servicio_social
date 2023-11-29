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
#importar todos los cvs con datos de registro y extraer pais por pais para graf
setwd("C:/serviciosocial/DATOS")
library(readxl)

Primn <- read.csv("C:/serviciosocial/proporciones/primn.csv")
Primf <- read.csv("C:/serviciosocial/proporciones/primf.csv")
Urban <- read.csv("C:/serviciosocial/proporciones/urban.csv")
lista<-list(Primf,Primn,Urban)
filtered_dfs <- lapply(lista, function(df) {
  filter(df, X == "Mexico")
})
merged_df <- bind_rows(filtered_dfs)
nuevos_nombres <- c("primn", "primf", "urban")
row.names(merged_df)[1:3] <- nuevos_nombres
merged_df<-merged_df[,-1]

library(ggplot2)
merged_df<-t(merged_df)
ruta<- "C:\\serviciosocial"
write.csv(merged_df, file.path(ruta, "prueba.csv")) ##

###########PRUEBA""""
Prueba <- read.csv("C:/serviciosocial/prueba.csv")

matplot(Prueba$X, Prueba[, -c(1,5)], type = "l", col = c("red", "blue", "green"),
        lty = 1, lwd = 2, xlab = "Año", ylab = "Valor",
        main = "Gráfica de Líneas para Tres Variables")
plot(Prueba$X, Prueba$primn, ylim = c(0,1), type = "l", col = "blue")
points(Prueba$X, Prueba$primf, col = "red", pch = 2)
legend("topright", legend = c("primn", "primf"), col = c("blue", "red"), lty = 1, pch = c(1, 2))







